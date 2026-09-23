000100 01  W479019-CTX.                                                         
000200*                                 ORDERBEKRÄFTELSE, FICHE                 
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 IDDC                 PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDLOPNR              PIC S9(3)           COMP-3.                  
001200*                                 LÖPNUMMER                               
001300     03 IDSEKVNR             PIC S9(3)           COMP-3.                  
001400*                                 GENERELLT SEKVENSNUMMER                 
001500     03 KDORDBEK             PIC 9(2).                                    
001600*                                 ORDERBEKRÄFTELSEKOD                     
001700     03 IDORDER              PIC S9(7)           COMP-3.                  
001800*                                 VOLVO PARTS ORDERNUMMER                 
001900     03 BEERS                PIC X(20).                                   
002000*                                 ERSÄTTNINGSTEXT                         
002100     03 ADGODSM-RAD1         PIC X(27).                                   
002200*                                 GODSMOTTAGARADRESS RAD 1                
002300     03 ADGODSM-RAD2         PIC X(27).                                   
002400*                                 GODSMOTTAGARADRESS RAD 2                
002500     03 BEGODSM-RAD1         PIC X(27).                                   
002600*                                 GODSMOTTAGARNAMN RAD 1                  
002700     03 BEGODSM-RAD2         PIC X(27).                                   
002800*                                 GODSMOTTAGARNAMN RAD 2                  
002900     03 BEKUNDRF             PIC X(15).                                   
003000*                                 KUNDENS REFERENS                        
003100     03 DIERS-KVOT           PIC S9(4)V9(3)      COMP-3.                  
003200*                                 KVOT MELLAN                             
003300*                                 DIERS-TILLK OCH DIERS-ERS               
003400     03 IDARTNR              PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600     03 IDARTNR-TILLK        PIC S9(9)           COMP-3.                  
003700*                                 TILLKOMMANDE ARTIKELNUMMER              
003800     03 IDKUNDRF-RO          PIC X(10).                                   
003900*                                 KUND REF PÅ RO                          
004000     03 IDSKYLT              PIC X(3).                                    
004100*                                 NATIONALITETSTECKEN                     
004200*                                 SPRÅKIDENTIFIKATION                     
004300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
004400*                                 FRAKTSÄTT DC TILL KUND                  
004500     03 KDORDKL              PIC S9              COMP-3.                  
004600*                                 ORDERKLASS                              
004700     03 KDTPOTYP             PIC S9              COMP-3.                  
004800*                                 TYP AV TIDPLANERAD ORDER                
004900     03 KVANNANT             PIC S9(7)           COMP-3.                  
005000*                                 ANNULLERAT ANTAL ARTIKLAR               
005100     03 KVBEART              PIC S9(7)           COMP-3.                  
005200*                                 BESTÄLLT ANTAL STYCKEN                  
005300     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
005400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
005500     03 KVBEART-TILLK        PIC S9(7)           COMP-3.                  
005600*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
005700     03 KVPRERO              PIC S9(7)           COMP-3.                  
005800*                                 PRELIMINÄR RO-KVANT                     
005900     03 KVRO                 PIC S9(7)           COMP-3.                  
006000*                                 ANTAL RESTNOTERADE ARTIKLAR             
006100     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
006200*                                 ANTAL I Q1 FÖRPACKNING                  
006300     03 REKSIFFR             PIC S9              COMP-3.                  
006400*                                 KONTROLLSIFFRA                          
006500     03 REKSIFFR-TILLK       PIC S9              COMP-3.                  
006600*                                 TILLKOMMANDE KONTROLLSIFFRA             
006700     03 TIREGDAT             PIC S9(7)           COMP-3.                  
006800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006900     03 TITPO                PIC S9(7)           COMP-3.                  
007000*                                 PLANERAD ORDERDATUM                     
007100*** END OF VILMAII-COPY LENGTH= 240 BYTES                                 
