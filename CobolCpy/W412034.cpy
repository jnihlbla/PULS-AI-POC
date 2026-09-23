000100 01  W412034.                                                             
000200*                                 PRINT POST FRÅN WDQ1                    
000300*                                                                         
000400     03 IDDISTR              PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 IDKUNDRF             PIC X(10).                                   
000900*                                 KUNDENS REFERENS (ORDERID)              
001000     03 IDORDER              PIC S9(7)           COMP-3.                  
001100*                                 VOLVO PARTS ORDERNUMMER                 
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 KDORDBEK             PIC 9(2).                                    
001700*                                 ORDERBEKRÄFTELSEKOD                     
001800     03 BEERS                PIC X(20).                                   
001900*                                 ERSÄTTNINGSTEXT                         
002000     03 BEKUNDRF             PIC X(15).                                   
002100*                                 KUNDENS REFERENS                        
002200     03 DIERS-KVOT           PIC S9(4)V9(3)      COMP-3.                  
002300*                                 KVOT MELLAN                             
002400*                                 DIERS-TILLK OCH DIERS-ERS               
002500     03 IDARTNR-TILLK        PIC S9(9)           COMP-3.                  
002600*                                 TILLKOMMANDE ARTIKELNUMMER              
002700     03 IDKUNDRF-RO          PIC X(10).                                   
002800*                                 KUND REF PÅ RO                          
002900     03 KDTPOTYP             PIC S9              COMP-3.                  
003000*                                 TYP AV TIDPLANERAD ORDER                
003100     03 KVANNANT             PIC S9(7)           COMP-3.                  
003200*                                 ANNULLERAT ANTAL ARTIKLAR               
003300     03 KVBEART              PIC S9(7)           COMP-3.                  
003400*                                 BESTÄLLT ANTAL STYCKEN                  
003500     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
003600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003700     03 KVBEART-TILLK        PIC S9(7)           COMP-3.                  
003800*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
003900     03 KVPRERO              PIC S9(7)           COMP-3.                  
004000*                                 PRELIMINÄR RO-KVANT                     
004100     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
004200*                                 ANTAL I Q1 FÖRPACKNING                  
004300     03 KVRO                 PIC S9(7)           COMP-3.                  
004400*                                 ANTAL RESTNOTERADE ARTIKLAR             
004500     03 REKSIFFR             PIC S9              COMP-3.                  
004600*                                 KONTROLLSIFFRA                          
004700     03 REKSIFFR-TILLK       PIC S9              COMP-3.                  
004800*                                 TILLKOMMANDE KONTROLLSIFFRA             
004900     03 TIDISPIN             PIC S9(7)           COMP-3.                  
005000*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
005100     03 TITPO                PIC S9(7)           COMP-3.                  
005200*                                 PLANERAD ORDERDATUM                     
005300     03 IDPTYP               PIC X(3).                                    
005400*                                 POSTTYP                                 
005500     03 TIREGDAT             PIC S9(7)           COMP-3.                  
005600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005700     03 TIORDREG             PIC S9(7)           COMP-3.                  
005800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005900     03 KDORDKL              PIC S9              COMP-3.                  
006000*                                 ORDERKLASS                              
006100     03 KDBEKALT             PIC S9              COMP-3.                  
006200*                                 ORDERBEKRÄFTELSEALTERNATIV              
006300     03 IDBIL.                                                            
006400*                                 BILIDENTITET                            
006500        05 IDBILTYP          PIC X(3).                                    
006600*                                 BILTYP                                  
006700        05 TIAAAA            PIC X(4).                                    
006800*                                 ÅRTAL (ÅÅÅÅ)                            
006900        05 IDCHASSI-PIE      PIC X(6).                                    
007000*                                 CHASSINUMMER PIE                        
007100*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
