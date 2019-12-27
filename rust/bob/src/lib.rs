pub fn reply(message: &str) -> &str {
    match message.trim() {
        "" => "Fine. Be that way!",
        remark => respond_to_verbal_remark(remark),
    }
}

fn respond_to_verbal_remark(remark: &str) -> &str {
    match (is_question(remark), is_yelling(remark)) {
        (true, true) => "Calm down, I know what I'm doing!",
        (true, false) => "Sure.",
        (false, true) => "Whoa, chill out!",
        (false, false) => "Whatever.",
    }
}

fn is_question(remark: &str) -> bool {
    remark.ends_with('?')
}

fn is_yelling(remark: &str) -> bool {
    remark == remark.to_uppercase() && remark != remark.to_lowercase()
}
