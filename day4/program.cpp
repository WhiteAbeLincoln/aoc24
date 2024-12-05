#include <cstdio>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>

std::vector<std::string> read_lines() {
  std::vector<std::string> ret{};

  for (std::string line; std::getline(std::cin, line);) {
    ret.emplace_back(std::move(line));
  }

  return ret;
}

size_t check_direction(
  std::string_view letters,
  const std::vector<std::string> &lines,
  int row, int col,
  int row_shift, int col_shift
) {
  int letter = 0;
  // this is already handled in the for loop
  // when i = 0
  // if (row < 0 || row >= lines.size()) {
  //   return 0;
  // }
  // if (col < 0 || col >= lines[row].size()) {
  //   return 0;
  // }

  for (int i = 0; i < letters.size(); ++i) {
    int r = row + i * row_shift;
    int c = col + i * col_shift;

    if (r < 0 || r >= lines.size() ||
        c < 0 || c >= lines[r].size() ||
        letters[i] != lines[r][c]) {
      return 0;
    }
  }

  return 1;
}

int check_xmas(const std::vector<std::string> lines, int row, int col) {
  return (
    check_direction("XMAS", lines, row, col, 0, 1) +  // check right
    check_direction("XMAS", lines, row, col, 0, -1) + // check left
    check_direction("XMAS", lines, row, col, -1, 0) + // check up
    check_direction("XMAS", lines, row, col, 1, 0) +  // check down
    check_direction("XMAS", lines, row, col, -1, -1) + // check diagonal left up
    check_direction("XMAS", lines, row, col, 1, -1) + // check diagonal left down
    check_direction("XMAS", lines, row, col, -1, 1) + // check diagonal right up
    check_direction("XMAS", lines, row, col, 1, 1) // check diagonal right down
  );
}

int check_x_mas(const std::vector<std::string> lines, int row, int col) {
  // we're given the row, col of the A
  // we must check the upper right and the upper left
  // for a valid M A S

  // only one of MAS or SAM can match, so adding is essentially checking if either match.
  // we shift our start position from the A in the center to the
  // letter up one row and to the left one row then we check from that position
  // to the diagonal right down
  int ul_count = check_direction("MAS", lines, row - 1, col - 1, 1, 1) +
                 check_direction("SAM", lines, row - 1, col - 1, 1, 1);

  // we shift our start position from the A in the center to the letter up one
  // row and to the right one row then we check from that position to the
  // diagonal left down
  int ur_count = check_direction("MAS", lines, row - 1, col + 1, 1, -1) +
                 check_direction("SAM", lines, row - 1, col + 1, 1, -1);

  // now both must be set for a valid X-MAS
  return ul_count & ur_count;
}

int main() {
  const auto lines = read_lines();
  const size_t rows = lines.size();

  size_t xmas_count = 0;
  size_t x_mas_count = 0;

  for (size_t row = 0; row < rows; ++row) {
    std::string_view line = lines[row];

    // loop over the line until we find an X
    size_t col = 0;
    for (char c : line) {
      if (c == 'X') {
        xmas_count += check_xmas(lines, row, col);
      }
      if (c == 'A') {
        x_mas_count += check_x_mas(lines, row, col);
      }
      col++;
    }
  }

  std::printf("xmas count: %zu\n", xmas_count);
  std::printf("x-mas count: %zu\n", x_mas_count);

  return 0;
}
