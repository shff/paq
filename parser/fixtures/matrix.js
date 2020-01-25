Meshes.createBoxEx = function(box, inward) {
  if(!box)throw new Error();
  const dims = MathUtil.boxDimensions(box);
  if(dims[0] < 0 || dims[1] < 0 || dims[2] < 0)throw new Error();
  const posNormal = inward ? -1.0 : 1.0;
  const negNormal = inward ? 1.0 : -1.0;
  const vertices = [
    box[0], box[1], box[5], negNormal, 0.0, 0.0, 1.0, 0.0,
    box[0], box[4], box[5], negNormal, 0.0, 0.0, 1.0, 1.0,
    box[0], box[4], box[2], negNormal, 0.0, 0.0, 0.0, 1.0,
    box[0], box[1], box[2], negNormal, 0.0, 0.0, 0.0, 0.0,
    box[3], box[1], box[2], posNormal, 0.0, 0.0, 1.0, 0.0,
    box[3], box[4], box[2], posNormal, 0.0, 0.0, 1.0, 1.0,
    box[3], box[4], box[5], posNormal, 0.0, 0.0, 0.0, 1.0,
    box[3], box[1], box[5], posNormal, 0.0, 0.0, 0.0, 0.0,
    box[3], box[1], box[2], 0.0, negNormal, 0.0, 1.0, 0.0,
    box[3], box[1], box[5], 0.0, negNormal, 0.0, 1.0, 1.0,
    box[0], box[1], box[5], 0.0, negNormal, 0.0, 0.0, 1.0,
    box[0], box[1], box[2], 0.0, negNormal, 0.0, 0.0, 0.0,
    box[3], box[4], box[5], 0.0, posNormal, 0.0, 1.0, 0.0,
    box[3], box[4], box[2], 0.0, posNormal, 0.0, 1.0, 1.0,
    box[0], box[4], box[2], 0.0, posNormal, 0.0, 0.0, 1.0,
    box[0], box[4], box[5], 0.0, posNormal, 0.0, 0.0, 0.0,
    box[0], box[1], box[2], 0.0, 0.0, negNormal, 1.0, 0.0,
    box[0], box[4], box[2], 0.0, 0.0, negNormal, 1.0, 1.0,
    box[3], box[4], box[2], 0.0, 0.0, negNormal, 0.0, 1.0,
    box[3], box[1], box[2], 0.0, 0.0, negNormal, 0.0, 0.0,
    box[3], box[1], box[5], 0.0, 0.0, posNormal, 1.0, 0.0,
    box[3], box[4], box[5], 0.0, 0.0, posNormal, 1.0, 1.0,
    box[0], box[4], box[5], 0.0, 0.0, posNormal, 0.0, 1.0,
    box[0], box[1], box[5], 0.0, 0.0, posNormal, 0.0, 0.0];
  const indices = [0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7, 8, 9, 10, 8, 10, 11, 12,
    13, 14, 12, 14, 15, 16, 17, 18, 16, 18, 19, 20, 21, 22, 20, 22, 23];
  return MeshBuffer.fromPositionsNormalsUV(vertices, indices);
};
