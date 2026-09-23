000100 01  HEAD-WL01331.                                                        
000200*                                 COPYTEXT FOR PACKING SPECIFICAT         
000300*                                 ION LDC HEAD LINE                       
000400     03 HEAD-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 HEAD-ADGMT.                                                       
000700*                                 GODSMOTTAGARADRESS                      
000800        05 HEAD-ADGMT-GATA   PIC X(35).                                   
000900*                                 GODSMOTTAGARADRESS GATA                 
001000        05 HEAD-ADGMT-PADR   PIC X(35).                                   
001100*                                 GODSMOTTAGARADRESS POSTADRESS           
001200        05 HEAD-ADPOST-PNRORT REDEFINES HEAD-ADGMT-PADR.                  
001300*                                 POSTNUMMER + ORT                        
001400           07 HEAD-ADPOSTNR  PIC X(10).                                   
001500*                                 POSTNUMMER I ADRESS                     
001600           07 HEAD-ADCITY    PIC X(25).                                   
001700*                                 BENÄMNING PÅ STAD                       
001800        05 HEAD-ADPOST-ORTPNR REDEFINES HEAD-ADGMT-PADR.                  
001900*                                 ORT + POSTNUMMER                        
002000           07 HEAD-ADCITY    PIC X(25).                                   
002100*                                 BENÄMNING PÅ STAD                       
002200           07 HEAD-ADPOSTNR  PIC X(10).                                   
002300*                                 POSTNUMMER I ADRESS                     
002400        05 HEAD-ADGMT-LAND   PIC X(35).                                   
002500*                                 GODSMOTTAGARADRESS LAND                 
002600     03 HEAD-KDFRAKT         PIC Z9.                                      
002700*                                 FRAKTSÄTT DC TILL KUND                  
002800     03 HEAD-BEFRAKT         PIC X(20).                                   
002900*                                 FRAKT TEXT                              
003000     03 HEAD-BEGMRK-RAD1     PIC X(30).                                   
003100*                                 GODSMÄRKE  RAD1                         
003200     03 HEAD-BEGMRK-RAD2     PIC X(30).                                   
003300*                                 GODSMÄRKE  RAD2                         
003400     03 HEAD-BEFDKRAV        PIC X(40).                                   
003500*                                 FÖRRÅDSDATAKRAV                         
003600     03 HEAD-BEGMT.                                                       
003700*                                 GODSMOTTAGARNAMN                        
003800        05 HEAD-BEGMT-RAD1   PIC X(35).                                   
003900*                                 GODSMOTTAGARNAMN RAD 1                  
004000        05 HEAD-BEGMT-RAD2   PIC X(35).                                   
004100*                                 GODSMOTTAGARNAMN RAD 2                  
004200     03 HEAD-BELAGINS-GRP.                                                
004300*                                 LAGERINSTRUKTIONER                      
004400        05 HEAD-BELAGINS-DEL1                                             
004500                             PIC X(60).                                   
004600*                                 DEL AV LAGERINSTRUKTION                 
004700        05 HEAD-BELAGINS-DEL2                                             
004800                             PIC X(60).                                   
004900*                                 DEL AV LAGERINSTRUKTION                 
005000     03 HEAD-FLCOD           PIC X.                                       
005100*                                 KONTANTBETALANDE KUND                   
005200     03 HEAD-IDDISTR         PIC Z(3)9.                                   
005300*                                 DISTRIKTNUMMER                          
005400     03 HEAD-IDKUNDNR        PIC Z(5)9.                                   
005500*                                 KUNDNUMMER                              
005600     03 HEAD-IDKUNDRF        PIC X(10).                                   
005700*                                 KUNDENS REFERENS (ORDERID)              
005800     03 HEAD-KDORDKL         PIC 9.                                       
005900*                                 ORDERKLASS                              
006000     03 HEAD-IDBORD          PIC X(3).                                    
006100*                                 PACK-BORD                               
006200     03 HEAD-IDUSER          PIC X(8).                                    
006300*                                 ANVÄNDARENS SÄKERHETS ID                
006400     03 HEAD-IDPRODNR        PIC Z(6)9.                                   
006500*                                 PRODUKTIONSNUMMER                       
006600     03 HEAD-IDPLKLST        PIC Z(2)9.                                   
006700*                                 PLOCKLISTNUMMER                         
006800     03 HEAD-IDPRC.                                                       
006900*                                 PRODUKTIONSKANAL                        
007000        05 HEAD-IDPRCBAS     PIC X(3).                                    
007100*                                 PRC-BAS                                 
007200        05 HEAD-IDPRCVAR     PIC X.                                       
007300*                                 PRC-VARIANT                             
007400     03 HEAD-IDLOPNR-PL      PIC 9(3).                                    
007500*                                 PLOCKSATSENS LÖPNUMMER INOM             
007600*                                 PRC-GRUPP                               
007700     03 HEAD-IDLOPNR-ORD     PIC 9(3).                                    
007800*                                 ORDERNS ORDNINGSNUMMER INOM             
007900*                                 EN PLOCKSATS                            
008000     03 HEAD-IDZON           PIC X(2).                                    
008100*                                 TRANSPORTVÄG (RUTT,ZON)                 
008200     03 HEAD-TIREGDAT        PIC 9(6).                                    
008300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008400     03 HEAD-HOUR            PIC X(2).                                    
008500     03 HEAD-MINUTE          PIC X(2).                                    
008600     03 HEAD-BETEXT          PIC X(30).                                   
008700     03 HEAD-TIUTSKR         PIC 9(6).                                    
008800*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
008900     03 HEAD-HOUR-PRINT      PIC X(2).                                    
009000     03 HEAD-MINUTE-PRINT    PIC X(2).                                    
009100     03 HEAD-TIRFSDAT        PIC 9(6).                                    
009200*                                 KLART FÖR TRANSPORT ÅÅMMDD              
009300     03 HEAD-TIRFSTID        PIC 9(4).                                    
009400*                                 KLART FÖR TRANSPORT (TTMM)              
009500     03 HEAD-ADFLGEO         PIC X(3).                                    
009600*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
009700     03 HEAD-ADFLOMR         PIC Z(2)9.                                   
009800*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
009900     03 HEAD-ADRUTNIV        PIC Z(2)9.                                   
010000*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
010100     03 HEAD-BERADREF        PIC X(10).                                   
010200*                                 KUNDENS RADREFERENS                     
010300     03 HEAD-IDDEPT          PIC 9(2).                                    
010400*                                 AVDELNING I VERKSTAD                    
010500     03 HEAD-IDKOLLI         PIC Z(4)9.                                   
010600*                                 KOLLINUMMER                             
010700     03 HEAD-VKORDBTO        PIC Z(5)9.9.                                 
010800*                                 ORDERVIKT BRUTTO (KG)                   
010900     03 HEAD-BARCODE         PIC X(22).                                   
011000     03 HEAD-RESTORDER       PIC X(2).                                    
011100     03 HEAD-BEKUNDRF        PIC X(15).                                   
011200*                                 KUNDENS REFERENS                        
011300     03 HEAD-IDBILREG        PIC X(10).                                   
011400*                                 BILENS REGISTRERINGSNUMMER              
011500     03 HEAD-TIREPDAT        PIC Z(6).                                    
011600*                                 REPAIR DATE                             
011700     03 HEAD-FLFPLOCK        PIC X.                                       
011800*                                 FÖRLEVERANSINDIKATOR                    
011900     03 HEAD-FLTACDISKND     PIC X.                                       
012000     03 HEAD-KUNDINFO-RAD1   PIC X(35).                                   
012100*                                 GODSMOTTAGARNAMN RAD 1                  
012200     03 HEAD-KUNDINFO-RAD2   PIC X(35).                                   
012300*                                 GODSMOTTAGARNAMN RAD 2                  
012400     03 HEAD-TETACDBO        PIC X(35).                                   
012500*                                 REFERENS BUTIK ORDER TACDIS             
012600     03 HEAD-BETELNR-TACD    PIC X(25).                                   
012700*                                 TELEFONNUMMER SMS BUTIKSORDER           
012800     03 HEAD-BEMEKAN         PIC X(15).                                   
012900*                                 FÖRVALD MEKANIKER/VERKSTAD              
013000     03 HEAD-HOUR-MINUTE-TACDIS                                           
013100                             PIC X(5).                                    
013200     03 HEAD-BEBETRAD-1      PIC X(35).                                   
013300     03 HEAD-BETELNR         PIC X(20).                                   
013400     03 HEAD-IDMAIL          PIC X(60).                                   
013500     03 HEAD-IDTRPTNR        PIC Z(2)9.                                   
013600*                                 TRANSPORTIDENTITET                      
013700*** END OF VILMAII-COPY LENGTH= 901 BYTES                                 
