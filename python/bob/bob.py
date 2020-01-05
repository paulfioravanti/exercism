def response(hey_bob: str) -> str:
    remark: str = hey_bob.strip()
    if not remark:
        return "Fine. Be that way!"
    return __respond_to_verbal_remark(remark)


def __respond_to_verbal_remark(remark: str) -> str:
    question: bool = __is_question(remark)
    yelling: bool = __is_yelling(remark)

    if question and yelling:
        return "Calm down, I know what I'm doing!"
    if question:
        return "Sure."
    if yelling:
        return "Whoa, chill out!"
    return "Whatever."


def __is_question(remark: str) -> bool:
    return remark.endswith("?")


def __is_yelling(remark: str) -> bool:
    return remark == remark.upper() and remark != remark.lower()
