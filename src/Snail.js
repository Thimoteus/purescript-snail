exports.unref = function (cp) {
  return function () {
    cp.unref();
    return {};
  }
}
