function formatDate(date) {
    return date.getFullYear() + "-" +
        (date.getMonth() + 1).toString().padStart(2, '0') + "-" +
        date.getDate().toString().padStart(2, '0') + ' ' +
        date.getHours().toString().padStart(2, '0') + ':' +
        date.getMinutes().toString().padStart(2, '0');
    
}

function formatDateIn(node) {
    let str = formatDate(new Date(node.dateTime));
    node.innerText = str;
}

function formatDates() {
    Array.from(document.getElementsByTagName("time"))
        .forEach(formatDateIn);
}

window.onload = function () {
    function run(f) {
        f(); document.body.addEventListener('htmx:after-swap', f);
    }

    run(formatDates);
};
