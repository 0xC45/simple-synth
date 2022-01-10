use clap::Parser;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::StreamConfig;
use crossbeam::channel;
use glfw_window::GlfwWindow as AppWindow;
use opengl_graphics::{GlGraphics, OpenGL};
use piston::event_loop::*;
use piston::input::*;
use piston::window::WindowSettings;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::str::FromStr;
use std::thread;

/// Simple synthesizer
#[derive(Parser, Debug)]
#[clap(about, version, author)]
pub struct Options {
    /// Enable debug logging
    #[clap(short, long, parse(from_occurrences))]
    debug: u64,

    /// Audio device to use
    device: Option<String>,

    /// Piano octave to synthesize
    #[clap(short, long, default_value_t = 3)]
    octave: u8,

    /// Waveform to synthesize
    #[clap(short, long, default_value_t = Waveform::SINE)]
    waveform: Waveform,
}

#[derive(Debug, Copy, Clone)]
pub enum Waveform {
    SINE,
    SQUARE,
    TRIANGLE,
    SAWTOOTH,
}

impl FromStr for Waveform {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "sine" => Ok(Waveform::SINE),
            "sin" => Ok(Waveform::SINE),
            "square" => Ok(Waveform::SQUARE),
            "squ" => Ok(Waveform::SQUARE),
            "triangle" => Ok(Waveform::TRIANGLE),
            "tri" => Ok(Waveform::TRIANGLE),
            "sawtooth" => Ok(Waveform::SAWTOOTH),
            "saw" => Ok(Waveform::SAWTOOTH),
            _ => Err(String::from("unrecognized waveform")),
        }
    }
}

impl Display for Waveform {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match &self {
            Waveform::SINE => "sine",
            Waveform::SQUARE => "square",
            Waveform::TRIANGLE => "triangle",
            Waveform::SAWTOOTH => "sawtooth",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub enum MidiEvent {
    NoteOn(u8),
    NoteOff(u8),
}

#[derive(Debug)]
struct Note {
    frequency: f32,
    waveform: Waveform,
    envelope: Envelope,
}

#[derive(Debug)]
struct Envelope {
    clock: f32,
    //attack_time: f32,
    //attack_amplitude: f32,
    //decay_time: f32,
    //sustain_amplitude: f32,
    //end_time: f32,
    //release_time: f32,
}

fn main() {
    let options = Options::parse();
    if options.debug > 0 {
        println!("Options: {:?}", options);
    }

    let (midi_tx, midi_rx) = channel::bounded(0);
    let (exit_tx, exit_rx) = channel::bounded(0);

    let audio_th = start_audio_thread(options, midi_rx, exit_rx);
    let graphics_th = start_graphics_thread(midi_tx);

    graphics_th.join().unwrap();
    exit_tx.send(true).expect("failed to send exit signal");
    audio_th.join().unwrap();
}

fn start_graphics_thread(midi_tx: channel::Sender<MidiEvent>) -> std::thread::JoinHandle<()> {
    thread::spawn(move || {
        let opengl = OpenGL::V3_2;
        let mut window: AppWindow = WindowSettings::new("Simple Synthesizer", [400, 400])
            .exit_on_esc(true)
            .graphics_api(opengl)
            .build()
            .unwrap();

        let ref mut gl = GlGraphics::new(opengl);
        let mut events = Events::new(EventSettings::new().lazy(true));
        while let Some(e) = events.next(&mut window) {
            if let Some(Button::Keyboard(key)) = e.press_args() {
                if let Some(midi_val) = key_to_note(key) {
                    midi_tx
                        .send(MidiEvent::NoteOn(midi_val))
                        .expect("failed to send midi on event");
                }
            };
            if let Some(Button::Keyboard(key)) = e.release_args() {
                if let Some(midi_val) = key_to_note(key) {
                    midi_tx
                        .send(MidiEvent::NoteOff(midi_val))
                        .expect("failed to send midi off event");
                }
            };
            if let Some(args) = e.render_args() {
                gl.draw(args.viewport(), |_, g| {
                    graphics::clear([1.0; 4], g);
                });
            }
        }
    })
}

fn key_to_note(key: Key) -> Option<u8> {
    match key {
        Key::Z => Some(0),
        Key::S => Some(1),
        Key::X => Some(2),
        Key::C => Some(3),
        Key::F => Some(4),
        Key::V => Some(5),
        Key::G => Some(6),
        Key::B => Some(7),
        Key::N => Some(8),
        Key::J => Some(9),
        Key::M => Some(10),
        Key::K => Some(11),
        Key::Comma => Some(12),
        Key::L => Some(13),
        Key::Period => Some(14),
        Key::Slash => Some(15),
        _ => None,
    }
}

pub struct AudioContext {
    octave: u8,
    waveform: Waveform,
    sample_rate: f32,
    num_channels: usize,
    active_notes: RefCell<HashMap<u8, Note>>,
}

fn start_audio_thread(
    options: Options,
    midi_rx: channel::Receiver<MidiEvent>,
    exit_rx: channel::Receiver<bool>,
) -> std::thread::JoinHandle<()> {
    let host = cpal::default_host();

    let device = match options.device {
        Some(ref name) => host
            .output_devices()
            .expect("no audio output devices")
            .find(|device| device.name().unwrap().eq(name)),
        None => host.default_output_device(),
    }
    .expect("failed to find audio output device");

    let config = device.default_output_config().unwrap();
    let sample_format = config.sample_format();
    let config: StreamConfig = config.into();

    if options.debug > 0 {
        println!("Config: {:?}", config);
    }

    let audio_ctx = AudioContext {
        octave: options.octave,
        waveform: options.waveform,
        sample_rate: config.sample_rate.0 as f32,
        num_channels: config.channels as usize,
        active_notes: RefCell::new(HashMap::new()),
    };

    thread::spawn(move || match sample_format {
        cpal::SampleFormat::F32 => {
            play_audio_stream::<f32>(options, audio_ctx, midi_rx, exit_rx, &device, &config)
        }
        cpal::SampleFormat::I16 => {
            play_audio_stream::<i16>(options, audio_ctx, midi_rx, exit_rx, &device, &config)
        }
        cpal::SampleFormat::U16 => {
            play_audio_stream::<u16>(options, audio_ctx, midi_rx, exit_rx, &device, &config)
        }
    })
}

pub fn play_audio_stream<T>(
    options: Options,
    mut audio_ctx: AudioContext,
    midi_rx: channel::Receiver<MidiEvent>,
    exit_rx: channel::Receiver<bool>,
    device: &cpal::Device,
    config: &cpal::StreamConfig,
) where
    T: cpal::Sample,
    T: Display,
{
    let audio_fn = move |output: &mut [T], _: &cpal::OutputCallbackInfo| {
        for frame in output.chunks_mut(audio_ctx.num_channels) {
            let v: T = cpal::Sample::from::<f32>(&next_value(&mut audio_ctx, &midi_rx));
            if options.debug > 1 {
                println!("{}", v);
            }
            for value in frame.iter_mut() {
                *value = v;
            }
        }
    };

    let err_fn = |err| eprintln!("audio stream error: {}", err);

    let stream = device
        .build_output_stream(config, audio_fn, err_fn)
        .unwrap();

    stream.play().unwrap();

    exit_rx.recv().unwrap();
}

fn next_value(audio_ctx: &mut AudioContext, midi_rx: &channel::Receiver<MidiEvent>) -> f32 {
    let active_notes = audio_ctx.active_notes.get_mut();

    match midi_rx.try_recv() {
        Ok(MidiEvent::NoteOn(note_num)) => {
            if !active_notes.contains_key(&note_num) {
                let twelfth_root_of_two = 2_f32.powf(1_f32 / 12_f32);
                let octave_base_frequency = 27.5 * 2_f32.powf(audio_ctx.octave as f32);
                let frequency = octave_base_frequency * twelfth_root_of_two.powf(note_num as f32);
                let note = Note {
                    frequency,
                    waveform: audio_ctx.waveform,
                    envelope: Envelope { clock: 0_f32 },
                };
                active_notes.insert(note_num, note);
            }
        }
        Ok(MidiEvent::NoteOff(note_num)) => {
            active_notes.remove(&note_num);
        }
        _ => {}
    }

    let mut value = 0_f32;

    for (_, mut note) in active_notes {
        note.envelope.clock = note.envelope.clock + 1_f32;

        use std::f32::consts::PI;
        const TAU: f32 = 2_f32 * PI;
        const AMPLITUDE_MODIFIER: f32 = 0.2;
        let period = note.envelope.clock / audio_ctx.sample_rate;

        let sample = match note.waveform {
            Waveform::SINE => (note.frequency * TAU * period).sin(),
            Waveform::SQUARE => match (note.frequency * TAU * period).sin() {
                i if i > 0_f32 => 1_f32,
                _ => -1_f32,
            },
            Waveform::TRIANGLE => (note.frequency * TAU * period).sin().asin() * (2_f32 / PI),
            Waveform::SAWTOOTH => (((note.frequency * period) % 1_f32) - 0.5) * 2_f32,
        };

        value += sample * AMPLITUDE_MODIFIER;
    }

    value
}
