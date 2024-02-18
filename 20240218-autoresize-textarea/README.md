# Autoresize `<textarea>`

*2024-02-19*

At work we wanted to automatically resize a HTML [`<textarea>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea) when its content exceeded its initial height.
Our first attempt was to get the `value` of the text area and calculate a new value for the `rows` attribute.
This worked for key-by-key input, but broke with pasting due to some details of [the framework we're using](https://reflex-frp.org/).
We then got sidetracked by how difficult it is to access the "post-paste" value of a text area (e.g. [this old StackOverflow question](https://stackoverflow.com/questions/9857801/how-to-get-the-new-value-of-a-textarea-input-field-on-paste)) because the [`paste` event](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/paste_event) runs before its target element is updated.

It all felt very weird to me, so I decided to play around with the problem outside of work.
I found that the [`input` event](https://developer.mozilla.org/en-US/docs/Web/API/Element/input_event) is suitable for this problem, because it is fired whenever a UI action changes the text area's content, including when the user pastes into the text area.
Then I found [this article](https://stephanwagner.me/auto-resizing-textarea-with-vanilla-javascript) which gives a general solution for the problem, which uses the element's `scrollHeight` to calculate the correct height on `input`.
Its use of `height = "auto"` before reading `scrollHeight` also means that the text area shrinks when content is removed, and it respects the minimum height given by the `rows` attribute and `min-height` style.
I don't like that it clobbers the `box-sizing` style (I think extensions like this should respect as much of the original styling as possible), so I made my version conditional on the `box-sizing` of the element.

Here's the final version:

```javascript
textArea.addEventListener(
  "input",
  (event) => {
    const computedStyles = window.getComputedStyle(event.target);
    
    const paddingYPixels = 
      parseInt(computedStyles.getPropertyValue("padding-top")) +
      parseInt(computedStyles.getPropertyValue("padding-bottom"));
    
    var offset = 0;
    const boxSizing = computedStyles.getPropertyValue("box-sizing");
    switch (boxSizing) {
      case "content-box":
        offset -= paddingYPixels;
        break;
      case "border-box":
        const borderTopPixels = parseInt(computedStyles.getPropertyValue("border-top"));
        const borderBottomPixels = parseInt(computedStyles.getPropertyValue("border-bottom"));
        offset += paddingYPixels + borderTopPixels + borderBottomPixels;
        break;
      default:
        console.warn(`Unknown box-sizing value: ${boxSizing}`);
        break;
    };
    
    event.target.style.height = "auto";
    event.target.style.height = `${event.target.scrollHeight + offset}px`;
  }
);
```
