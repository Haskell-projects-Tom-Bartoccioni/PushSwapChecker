##
## EPITECH PROJECT, 2019
## pushswap_checker
## File description:
## Makefile
##

SRC			=		pushswap_checker.hs

NAME		=		pushswap_checker

RM			=		rm -f

all:	$(NAME)

$(NAME):
		ghc -o $(NAME) $(SRC)

clean:
		$(RM) pushswap_checker.hi
		$(RM) pushswap_checker.o

fclean:	clean
		$(RM) $(NAME)

re:		fclean all
