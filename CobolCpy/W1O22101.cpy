000100 01  MOD-W1O22101.                                                        
000200*                                 MOD-COPYTEXT FÖR W1022100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-BELEVART-IN      PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001100     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400     03 MOD-1002-SATS-IN     PIC X.                                       
001500*                                 ALLMÄN SVARSFLAGGA                      
001600     03 MOD-KDPRODSL-IN      PIC X(2).                                    
001700*                                 PRODUKTSLAG                             
001800     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 MOD-IDARTNR-SPAR     PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-BELEVART-UT      PIC X(30).                                   
002300*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002400     03 MOD-ARTIKELNR REDEFINES MOD-BELEVART-UT.                          
002500*                                                                         
002600        05 MOD-IDARTNR-UT    PIC X(9).                                    
002700*                                 ARTIKELNUMMER                           
002800        05 MOD-FILLER        PIC X(21).                                   
002900     03 MOD-IDSKYLT-UT       PIC X(3).                                    
003000*                                 NATIONALITETSTECKEN                     
003100*                                 SPRÅKIDENTIFIKATION                     
003200     03 MOD-1002-SATS-UT     PIC X.                                       
003300*                                 ALLMÄN SVARSFLAGGA                      
003400     03 MOD-KDPRODSL-UT      PIC X(2).                                    
003500*                                 PRODUKTSLAG                             
003600     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
003700*                                 ARTIKELNUMMER                           
003800     03 MOD-KDSTRRAD-ENTER   PIC X.                                       
003900*                                 TYP AV STRUKTURRAD                      
004000     03 MOD-IDRADNR-ENTER    PIC 9(4).                                    
004100*                                 RADNUMMER                               
004200     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
004300*                                 ARTIKELNUMMER                           
004400     03 MOD-KDSTRRAD-NEXT    PIC X.                                       
004500*                                 TYP AV STRUKTURRAD                      
004600     03 MOD-IDRADNR-NEXT     PIC 9(4).                                    
004700*                                 RADNUMMER                               
004800     03 MOD-BEART-UT         PIC X(25).                                   
004900*                                 ARTIKELBENÄMNING                        
005000     03 MOD-TEARTNOT         PIC X(40).                                   
005100*                                 ARTIKEL NOTERING                        
005200     03 MOD-TEARTNOT-7       PIC X(40).                                   
005300*                                 ARTIKEL NOTERING                        
005400     03 MOD-RADER            OCCURS 10 TIMES.                             
005500*                                 RADINFORMATION                          
005600        05 MOD-SELECT-ATTR   PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-SELECT        PIC X.                                       
005900        05 MOD-IDRADNR       PIC Z(3)9.                                   
006000*                                 RADNUMMER                               
006100        05 MOD-IDARTNR       PIC Z(8)9.                                   
006200*                                 ARTIKELNUMMER                           
006300        05 MOD-BEART         PIC X(25).                                   
006400*                                 ARTIKELBENÄMNING                        
006500        05 MOD-REANTPSA      PIC Z9.9(3).                                 
006600*                                 ANTAL PER SATS                          
006700        05 MOD-IDSTRTYP      PIC X.                                       
006800*                                 STRUKTURTYP                             
006900        05 MOD-KDERS         PIC Z9.                                      
007000*                                 ERSÄTTNINGSKOD                          
007100        05 MOD-IDLEVNR       PIC X(5).                                    
007200*                                 LEVERANTÖRNUMMER                        
007300        05 MOD-KDPRODSL      PIC Z9.                                      
007400*                                 PRODUKTSLAG                             
007500        05 MOD-KDPSLLOC      PIC 9(2).                                    
007600*                                 PRODUKTSLAG LOKALT                      
007700     03 MOD-KDPRTVAL-ATTR    PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 MOD-KDPRTVAL         PIC X.                                       
008000*                                 PRINTER-VAL KOD                         
008100     03 MOD-TEMFSINF         PIC X(55).                                   
008200*                                 INFORMATIONSMEDDELANDE                  
008300*** END OF VILMAII-COPY LENGTH= 916 BYTES                                 
