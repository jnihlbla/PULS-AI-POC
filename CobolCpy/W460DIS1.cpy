000100 01  DIS1-W460DIS1.                                                       
000200*                                 PARAMETRAR TILL SUBPROGRAMMET           
000300*                                 W460DIS1 SOM UNDERSÖKER OM ETT          
000400*                                 DISTRIKT ÄR ETT NOAC-DISTRIKT.          
000500*                                 SUBPROGRAMMET TAR DISTRIKT SOM          
000600*                                 INPUT OCH LÄMNAR SVAR I DE TRE          
000700*                                 ANDRA FÄLTEN.                           
000800*                                 KDSVAR:  J = NOAC-DISTR,                
000900*                                          N = EJ N-D                     
001000*                                 IDLANDX2 = ISO TVÅ-STÄLLIG              
001100*                                 LANDSKOD. S-LAGREN GER S1/S2.           
001200*                                 OM DISTRIKTET INTE ÄR ETT               
001300*                                 NOAC-DISTRIKT BLANKAS IDLANDX2          
001400*                                                                         
001500     03 DIS1-IDDISTR         PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700     03 DIS1-KDSVAR          PIC X.                                       
001800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001900     03 DIS1-IDLANDX2        PIC X(2).                                    
002000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002100*** END OF VILMAII-COPY LENGTH= 6 BYTES                                   
