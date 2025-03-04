#include <iostream>
#include <stdexcept>
#include <string>
#include <cmath>
#include <sstream>

// Token types for arithmetic expressions
enum class TokenType {
    NUMBER,
    ADD,
    SUB,
    MUL,
    DIV,
    POW,
    SQRT,
    FACT,
    PI,
    SIN,
    COS,
    TAN,
    ASIN,
    ACOS,
    ATAN,
    PERCENT,
    LPAREN,
    RPAREN,
    DEG,
    END
};

// Token structure for arithmetic expressions
struct Token {
    TokenType type;
    double value;
};

// Lexer to tokenize input for arithmetic expressions
class Lexer {
public:
    Lexer(const std::string& input) : input_(input), pos_(0) {}

    Token getNextToken() {
        while (pos_ < input_.size() && std::isspace(input_[pos_])) {
            ++pos_;
        }

        if (pos_ >= input_.size()) {
            return {TokenType::END, 0};
        }

        if (std::isdigit(input_[pos_])) {
            size_t start = pos_;
            while (pos_ < input_.size() && (std::isdigit(input_[pos_]) || input_[pos_] == '.')) {
                ++pos_;
            }
            double value = std::stod(input_.substr(start, pos_ - start));
            return {TokenType::NUMBER, value};
        }

        switch (input_[pos_]) {
            case '+':
                ++pos_;
                return {TokenType::ADD, 0};
            case '-':
                ++pos_;
                return {TokenType::SUB, 0};
            case '*':
                ++pos_;
                return {TokenType::MUL, 0};
            case '/':
                ++pos_;
                return {TokenType::DIV, 0};
            case '^':
                ++pos_;
                return {TokenType::POW, 0};
            case '%':
                ++pos_;
                return {TokenType::PERCENT, 0};
            case '(':
                ++pos_;
                return {TokenType::LPAREN, 0};
            case ')':
                ++pos_;
                return {TokenType::RPAREN, 0};
            case '!':
                ++pos_;
                return {TokenType::FACT, 0};
            default:
                if (input_.substr(pos_, 2) == "pi") {
                    pos_ += 2;
                    return {TokenType::PI, 0};
                } else if (input_.substr(pos_, 3) == "sin") {
                    pos_ += 3;
                    return {TokenType::SIN, 0};
                } else if (input_.substr(pos_, 3) == "cos") {
                    pos_ += 3;
                    return {TokenType::COS, 0};
                } else if (input_.substr(pos_, 3) == "tan") {
                    pos_ += 3;
                    return {TokenType::TAN, 0};
                } else if (input_.substr(pos_, 4) == "asin") {
                    pos_ += 4;
                    return {TokenType::ASIN, 0};
                } else if (input_.substr(pos_, 4) == "acos") {
                    pos_ += 4;
                    return {TokenType::ACOS, 0};
                } else if (input_.substr(pos_, 4) == "atan") {
                    pos_ += 4;
                    return {TokenType::ATAN, 0};
                } else if (input_.substr(pos_, 4) == "sqrt") {
                    pos_ += 4;
                    return {TokenType::SQRT, 0};
                } else if (input_.substr(pos_, 3) == "deg") {
                    pos_ += 3;
                    return {TokenType::DEG, 0};
                }
                throw std::invalid_argument("Invalid character");
        }
    }

private:
    const std::string& input_;
    size_t pos_;
};

// Parser to evaluate arithmetic expressions
class Parser {
public:
    Parser(const std::string& input) : lexer_(input), currentToken_(lexer_.getNextToken()) {}

    double parse() {
        return parseExpression();
    }

private:
    double parseExpression() {
        double result = parseTerm();
        while (currentToken_.type == TokenType::ADD || currentToken_.type == TokenType::SUB) {
            if (currentToken_.type == TokenType::ADD) {
                getNextToken();
                result += parseTerm();
            } else {
                getNextToken();
                result -= parseTerm();
            }
        }
        return result;
    }

    double parseTerm() {
        double result = parseFactor();
        while (currentToken_.type == TokenType::MUL || currentToken_.type == TokenType::DIV) {
            if (currentToken_.type == TokenType::MUL) {
                getNextToken();
                result *= parseFactor();
            } else if (currentToken_.type == TokenType::DIV) {
                getNextToken();
                double divisor = parseFactor();
                if (divisor == 0) {
                    throw std::runtime_error("Division by zero");
                }
                result /= divisor;
            }
        }
        return result;
    }

    double parseFactor() {
        double result = parsePower();
        if (currentToken_.type == TokenType::FACT) {
            getNextToken();
            if (result < 0) {
                throw std::invalid_argument("Cannot compute factorial of negative number");
            }
            double factorialResult = 1;
            for (int i = 2; i <= static_cast<int>(result); ++i) {
                factorialResult *= i;
            }
            return factorialResult;
        }
        return result;
    }

    double parsePower() {
        double result = parseUnary();
        if (currentToken_.type == TokenType::POW) {
            getNextToken();
            double exponent = parseFactor();
            result = pow(result, exponent);
        }
        return result;
    }

    double parseUnary() {
        if (currentToken_.type == TokenType::ADD) {
            getNextToken();
            return +parsePrimary();
        }
        if (currentToken_.type == TokenType::SUB) {
            getNextToken();
            return -parsePrimary();
        }
        if (currentToken_.type == TokenType::PERCENT) {
            getNextToken();
            double value = parsePrimary();
            return value / 100.0;
        }
        return parsePrimary();
    }

    double parsePrimary() {
        if (currentToken_.type == TokenType::NUMBER) {
            double value = currentToken_.value;
            getNextToken();
            return value;
        } else if (currentToken_.type == TokenType::LPAREN) {
            getNextToken();
            double value = parseExpression();
            if (currentToken_.type != TokenType::RPAREN) {
                throw std::invalid_argument("Expected ')'");
            }
            getNextToken();
            return value;
        } else if (currentToken_.type == TokenType::SQRT) {
            getNextToken();
            if (currentToken_.type != TokenType::LPAREN) {
                throw std::invalid_argument("Expected '(' after sqrt");
            }
            getNextToken();
            double value = parseExpression();
            if (currentToken_.type != TokenType::RPAREN) {
                throw std::invalid_argument("Expected ')'");
            }
            getNextToken();
            if (value < 0) {
                throw std::invalid_argument("Cannot compute square root of negative number");
            }
            return sqrt(value);
        } else if (currentToken_.type == TokenType::PI) {
            getNextToken();
            return M_PI;
        } else if (currentToken_.type == TokenType::SIN) {
            getNextToken();
            if (currentToken_.type != TokenType::LPAREN) {
                throw std::invalid_argument("Expected '(' after sin");
            }
            getNextToken();
            double value = parseExpression();
            bool isDegrees = false;
            if (currentToken_.type == TokenType::DEG) {
                isDegrees = true;
                getNextToken();
            }
            if (currentToken_.type != TokenType::RPAREN) {
                throw std::invalid_argument("Expected ')'");
            }
            getNextToken();
            return isDegrees ? sin(value * M_PI / 180.0) : sin(value);
        } else if (currentToken_.type == TokenType::COS) {
            getNextToken();
            if (currentToken_.type != TokenType::LPAREN) {
                throw std::invalid_argument("Expected '(' after cos");
            }
            getNextToken();
            double value = parseExpression();
            bool isDegrees = false;
            if (currentToken_.type == TokenType::DEG) {
                isDegrees = true;
                getNextToken();
            }
            if (currentToken_.type != TokenType::RPAREN) {
                throw std::invalid_argument("Expected ')'");
            }
            getNextToken();
            return isDegrees ? cos(value * M_PI / 180.0) : cos(value);
        } else if (currentToken_.type == TokenType::TAN) {
            getNextToken();
            if (currentToken_.type != TokenType::LPAREN) {
                throw std::invalid_argument("Expected '(' after tan");
            }
            getNextToken();
            double value = parseExpression();
            bool isDegrees = false;
            if (currentToken_.type == TokenType::DEG) {
                isDegrees = true;
                getNextToken();
            }
            if (currentToken_.type != TokenType::RPAREN) {
                throw std::invalid_argument("Expected ')'");
            }
            getNextToken();
            return isDegrees ? tan(value * M_PI / 180.0) : tan(value);
        } else if (currentToken_.type == TokenType::ASIN) {
            getNextToken();
            if (currentToken_.type != TokenType::LPAREN) {
                throw std::invalid_argument("Expected '(' after asin");
            }
            getNextToken();
            double value = parseExpression();
            bool isDegrees = false;
            if (currentToken_.type == TokenType::DEG) {
                isDegrees = true;
                getNextToken();
            }
            if (currentToken_.type != TokenType::RPAREN) {
                throw std::invalid_argument("Expected ')'");
            }
            getNextToken();
            return isDegrees ? asin(value) * 180.0 / M_PI : asin(value);
        } else if (currentToken_.type == TokenType::ACOS) {
            getNextToken();
            if (currentToken_.type != TokenType::LPAREN) {
                throw std::invalid_argument("Expected '(' after acos");
            }
            getNextToken();
            double value = parseExpression();
            bool isDegrees = false;
            if (currentToken_.type == TokenType::DEG) {
                isDegrees = true;
                getNextToken();
            }
            if (currentToken_.type != TokenType::RPAREN) {
                throw std::invalid_argument("Expected ')'");
            }
            getNextToken();
            return isDegrees ? acos(value) * 180.0 / M_PI : acos(value);
        } else if (currentToken_.type == TokenType::ATAN) {
            getNextToken();
            if (currentToken_.type != TokenType::LPAREN) {
                throw std::invalid_argument("Expected '(' after atan");
            }
            getNextToken();
            double value = parseExpression();
            bool isDegrees = false;
            if (currentToken_.type == TokenType::DEG) {
                isDegrees = true;
                getNextToken();
            }
            if (currentToken_.type != TokenType::RPAREN) {
                throw std::invalid_argument("Expected ')'");
            }
            getNextToken();
            return isDegrees ? atan(value) * 180.0 / M_PI : atan(value);
        } else {
            throw std::invalid_argument("Expected number, '(' or function");
        }
    }

    void getNextToken() {
        currentToken_ = lexer_.getNextToken();
    }

    Lexer lexer_;
    Token currentToken_;
};

int main() {
    std::cout << "Enter 'END' to exit.\n";
    while (true) {
        std::cout << "\nEnter an arithmetic expression:\n";
        std::string input;
        std::getline(std::cin, input);

        if (input == "END") {
            break;
        }

        try {
            Parser parser(input);
            double result = parser.parse();
            std::cout << "Result: " << result << std::endl;
        } catch (const std::exception& e) {
            std::cerr << "Error: " << e.what() << std::endl;
        }
    }

    return 0;
}
