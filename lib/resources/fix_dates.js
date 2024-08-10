window.onload = function () {
    function formatDateIn(node) {
        let date = new Date(node.innerText);
        let str = date.getFullYear() + "-" +
            (date.getMonth() + 1).toString().padStart(2, '0') + "-" +
            date.getDay().toString().padStart(2, '0') + ' ' +
            date.getHours().toString().padStart(2, '0') + ':' +
            date.getMinutes().toString().padStart(2, '0');
        node.innerText = str;
    }

    function formatDates() {
        Array.from(document.getElementsByClassName("time"))
            .forEach(formatDateIn);
    }

    formatDates();
    document.body.addEventListener('htmx:after-swap', formatDates);

};
