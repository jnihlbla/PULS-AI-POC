000100 01  W22435-CTX.                                                          
000200*                                 NYA ARTIKLAR ELLER LEVERANTÖRS-         
000300*                                 BYTEN KOMPLETTERADE POSTER              
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BEART-SVE            PIC X(25).                                   
000700*                                 ARTIKELBENÄMNING                        
000800     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
000900*                                 FUNKTIONSGRUPP                          
001000     03 IDLEVNR-NY           PIC S9(5)           COMP-3.                  
001100*                                 LEVERANTÖRNUMMER                        
001200     03 IDLEVNR-GAM          PIC S9(5)           COMP-3.                  
001300*                                 LEVERANTÖRNUMMER                        
001400     03 BEFT                 PIC S9(3)           COMP-3.                  
001500*                                 FÖRPACKNINGSTYP                         
001600     03 TIFINLV              PIC S9(5)           COMP-3.                  
001700*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001800     03 IDPROJ               PIC X(4).                                    
001900*                                 PARTS PROJEKTIDENTITET                  
002000     03 NYREG                PIC X(2).                                    
002100     03 IDARTNR-MOTSV        PIC S9(9)           COMP-3.                  
002200*                                 MOTSVARANDE ARTIKEL                     
002300     03 KDFORP.                                                           
002400*                                 FÖRPACKNINGSKOD                         
002500        05 KDFORPPL          PIC 9.                                       
002600*                                 FÖRPACKNINGSPLATS                       
002700        05 KDFORPGP          PIC 9(2).                                    
002800*                                 FÖRPACKNINGSGRUPP                       
002900        05 KDFORPUF          PIC 9.                                       
003000*                                 UPPRÄKNINGSFAKTOR                       
003100     03 KDFARLIG             PIC S9              COMP-3.                  
003200*                                 KOD FÖR FARLIGT GODS                    
003300*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
