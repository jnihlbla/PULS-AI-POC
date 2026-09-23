000100 01  REQU-W50281I1.                                                       
000200*                                 REQUEST-COPYTEXT FÖR BILD 5281          
000300*                                 VID ANROP FRÅN WEBBEN                   
000400*                                 ACS         RE0                         
000500     03 REQU-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 REQU-IDTIDZON        PIC 9(2).                                    
000800*                                 TIDZONER PÅ JORDEN.                     
000900     03 REQU-ACS-RE0-GRP.                                                 
001000*                                 ACS GROUP                               
001100        05 REQU-PRAVCOST     PIC X(10).                                   
001200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
001300        05 REQU-PREXAVCOST   PIC X(16).                                   
001400*                                 ALLMÄNT BELOPPSFÄLT (KR O ÖREN)         
001500*                                                                         
001600        05 REQU-FLONHAND     PIC X.                                       
001700*                                 ALLMÄN FLAGGA                           
001800        05 REQU-FLBUFFER     PIC X.                                       
001900*                                 ALLMÄN FLAGGA                           
002000        05 REQU-FLACSLOAD    PIC X.                                       
002100*                                 ALLMÄN FLAGGA                           
002200*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
