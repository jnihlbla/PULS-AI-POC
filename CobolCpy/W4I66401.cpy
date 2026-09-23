000100 01  MID-W4I66401.                                                        
000200*                                 MID-COPYTEXT FÖR W40664                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500     03 MID-IDTRPTNR-UT      PIC X(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 MID-IDLBBET-IN       PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900     03 MID-IDLBBET-UT       PIC X(12).                                   
001000*                                 LASTBÄRARBETECKNING                     
001100     03 MID-FLFARLIG-IN      PIC X.                                       
001200*                                 FARLIGT GODS-FLAGGA                     
001300     03 MID-FLFARLIG-UT      PIC X.                                       
001400*                                 FARLIGT GODS-FLAGGA                     
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-IDDISTR-ENTER    PIC X(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MID-IDKUNDNR-ENTER   PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MID-KDFAKTYP-ENTER   PIC X.                                       
002400*                                 FAKTURATYP                              
002500     03 MID-IDORDNR7-ENTER   PIC X(7).                                    
002600*                                 ORDERNUMMER                             
002700     03 MID-IDPRODNR-ENTER   PIC X(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900     03 MID-IDKOLLI-ENTER    PIC X(5).                                    
003000*                                 KOLLINUMMER                             
003100     03 MID-IDDISTR-NEXT     PIC X(4).                                    
003200*                                 DISTRIKTNUMMER                          
003300     03 MID-IDKUNDNR-NEXT    PIC X(6).                                    
003400*                                 KUNDNUMMER                              
003500     03 MID-KDFAKTYP-NEXT    PIC X.                                       
003600*                                 FAKTURATYP                              
003700     03 MID-IDORDNR7-NEXT    PIC X(7).                                    
003800*                                 ORDERNUMMER                             
003900     03 MID-IDPRODNR-NEXT    PIC X(7).                                    
004000*                                 PRODUKTIONSNUMMER                       
004100     03 MID-IDKOLLI-NEXT     PIC X(5).                                    
004200*                                 KOLLINUMMER                             
004300     03 MID-FLAVSLUTA        PIC X.                                       
004400*                                 ALLMÄN FLAGGA                           
004500     03 MID-IDDISTR-DOLD     PIC 9(4).                                    
004600*                                 DISTRIKTNUMMER                          
004700     03 MID-FLLSTDOK         PIC X.                                       
004800*                                 ALLMÄN FLAGGA                           
004900     03 MID-FLTRPDOK         PIC X.                                       
005000*                                 ALLMÄN FLAGGA                           
005100     03 MID-FLPROFORMA       PIC X.                                       
005200*                                 ALLMÄN FLAGGA                           
005300     03 MID-RADER            OCCURS 10 TIMES.                             
005400*                                 RADER                                   
005500        05 MID-FLBACKA-RAD   PIC X.                                       
005600*                                 ALLMÄN FLAGGA                           
005700        05 MID-IDDISTR       PIC 9(4).                                    
005800*                                 DISTRIKTNUMMER                          
005900        05 MID-IDKUNDNR      PIC 9(6).                                    
006000*                                 KUNDNUMMER                              
006100        05 MID-KDFAKTYP      PIC X.                                       
006200*                                 FAKTURATYP                              
006300        05 MID-IDORDNR7      PIC 9(7).                                    
006400*                                 ORDERNUMMER                             
006500        05 MID-IDKOLLI       PIC 9(5).                                    
006600*                                 KOLLINUMMER                             
006700        05 MID-IDPRODNR      PIC 9(7).                                    
006800*                                 PRODUKTIONSNUMMER                       
006900     03 MID-IDKOLLI-SAMP-TAB.                                             
007000*                                 SPARAREA FÖR IDKOLLI-SAMP               
007100        05 MID-IDKOLLI-SAMP  OCCURS 10 TIMES                              
007200                             PIC 9(5).                                    
007300*                                 SAMPACKNINGSKOLLINUMMER                 
007400*** END OF VILMAII-COPY LENGTH= 464 BYTES                                 
