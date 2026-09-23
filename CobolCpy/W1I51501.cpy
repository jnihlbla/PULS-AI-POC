000100 01  MID-W1I51501.                                                        
000200*                                 MID-COPYTEXT F÷R BILD 1515              
000300*                                 VADIS/AVSNITT S÷KBEGREPP                
000400     03 MID-IDCATNR-IN       PIC X(5).                                    
000500*                                 KATALOG-ID                              
000600     03 MID-IDCATNR-UT       PIC X(5).                                    
000700*                                 KATALOG-ID                              
000800     03 MID-IDCATGRP-IN      PIC X(2).                                    
000900*                                 KATALOG-GRUPP                           
001000     03 MID-IDCATGRP-UT      PIC X(2).                                    
001100*                                 KATALOG-GRUPP                           
001200     03 MID-IDCATAVS-IN      PIC X(4).                                    
001300*                                 KATALOG-AVSNITT                         
001400     03 MID-IDCATAVS-UT      PIC X(4).                                    
001500*                                 KATALOG-AVSNITT                         
001600     03 MID-IDSKYLT-IN       PIC X(3).                                    
001700*                                 NATIONALITETSTECKEN                     
001800*                                 SPR≈KIDENTIFIKATION                     
001900     03 MID-IDSKYLT-UT       PIC X(3).                                    
002000*                                 NATIONALITETSTECKEN                     
002100*                                 SPR≈KIDENTIFIKATION                     
002200     03 MID-IDCATRAD-IN      PIC X(4).                                    
002300*                                 RADNUMMER                               
002400     03 MID-IDCATRAD-UT      PIC X(4).                                    
002500*                                 RADNUMMER                               
002600     03 MID-KDCATPUB-R-FOM-IN                                             
002700                             PIC X(3).                                    
002800*                                 3 H÷GRASTE TECKNEN I KDCATPUB           
002900     03 MID-KDCATPUB-R-FOM-UT                                             
003000                             PIC X(3).                                    
003100*                                 3 H÷GRASTE TECKNEN I KDCATPUB           
003200     03 MID-FLAVSTVAD        PIC X.                                       
003300*                                 AVSNITTET SKICKAS TILL VADIS?           
003400     03 MID-KDCATPUB-R-TOM   PIC X(3).                                    
003500*                                 3 H÷GRASTE TECKNEN I KDCATPUB           
003600     03 MID-RAD              OCCURS 10 TIMES.                             
003700*                                 INDATA-RADER F÷R VADIS                  
003800*                                 S÷KBEGREPP P≈ AVSNITT                   
003900        05 MID-IDKOL         PIC X.                                       
004000*                                 KOLUMN-ID (A-E)                         
004100        05 MID-FLEXCL        PIC X.                                       
004200*                                 NYCKELVƒRDEN EXCLUDERAS?                
004300        05 MID-IDMODELL      PIC X(3).                                    
004400*                                 BILENS NUMERISKA MODELLBET.             
004500        05 MID-TIMODAAR-STA  PIC X(4).                                    
004600*                                 MODELL≈R (≈≈≈≈) START≈R                 
004700        05 MID-TIMODAAR-STO  PIC X(4).                                    
004800*                                 MODELL≈R (≈≈≈≈) STOPP≈R                 
004900        05 MID-IDVARIANT     PIC X(15).                                   
005000*                                 BILVARIANT                              
005100        05 MID-IDVARIANT-2   PIC X(15).                                   
005200*                                 ASSOCIERAD (2:A) BILVARIANT             
005300        05 MID-KDCHATYP      PIC X.                                       
005400*                                 CHASSINUMMER-TYP                        
005500        05 MID-IDCHASSI-STA  PIC X(6).                                    
005600*                                 CHASSINUMMER START                      
005700        05 MID-IDCHASSI-STO  PIC X(6).                                    
005800*                                 CHASSINUMMER STOPP                      
005900        05 MID-IDRADNR       PIC X(4).                                    
006000*                                 RADNUMMER                               
006100*** END OF VILMAII-COPY LENGTH= 646 BYTES                                 
