000100 01  DET-WL01612.                                                         
000200*                                 COPYTEXT FOR DISCREPANCY INFO L         
000300*                                 DC DETAIL LINE                          
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 DET-KDANMORS         PIC X(2).                                    
000700*                                 ORSAK TILL LEVERANSANMÄRKNING           
000800     03 DET-KVBEART-Q        PIC Z(5)9.                                   
000900*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001000     03 DET-ADLAGOMR         PIC 9(2).                                    
001100*                                 LAGEROMRÅDE                             
001200     03 DET-ADGANG           PIC 9(2).                                    
001300*                                 GÅNG                                    
001400     03 DET-ADPLATS          PIC 9(5).                                    
001500*                                 LAGERPLATSNUMMER                        
001600     03 DET-IDORDNR7         PIC Z(6)9.                                   
001700*                                 ORDERNUMMER                             
001800     03 DET-KVLEVART         PIC Z(6)9.                                   
001900*                                 LEVERERAT ANTAL STYCK                   
002000     03 DET-KVLS             PIC -(7)9.                                   
002100*                                 LAGERSALDO                              
002200     03 DET-KVLEVANM-BEKR    PIC Z(5)9.                                   
002300*                                 BEKRÄFTAT RETURANTAL                    
002400     03 DET-VKORDBTO-KOLLI   PIC Z(5)9.9.                                 
002500*                                 ORDERVIKT BRUTTO PER KOLLI              
002600     03 DET-KVPB-TOT         PIC Z(5)9.9.                                 
002700*                                 TOTALT PERIODBEHOV                      
002800     03 DET-VKORDNTO-KOLLI   PIC Z(5)9.9.                                 
002900*                                 ORDERVIKT NETTO PER KOLLI               
003000     03 DET-KDERS            PIC Z9.                                      
003100*                                 ERSÄTTNINGSKOD                          
003200     03 DET-IDKOLLI          PIC Z(4)9.                                   
003300*                                 KOLLINUMMER                             
003400     03 DET-VKTARA           PIC Z(5)9.9.                                 
003500*                                 TARAVIKT (KG)                           
003600     03 DET-VKART            PIC Z(6)9.                                   
003700*                                 ARTIKELVIKT (G)                         
003800     03 DET-KDFAKTYP         PIC X.                                       
003900*                                 FAKTURATYP                              
004000     03 DET-IDFAKT           PIC Z(6)9.                                   
004100*                                 FAKTURANUMMER                           
004200     03 DET-VKORDNTO-TOT     PIC Z(5)9.9.                                 
004300*                                 ORDERVIKT NETTO (KG)                    
004400     03 DET-TIFAKT           PIC 9(6).                                    
004500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004600     03 DET-KDORDKL          PIC 9.                                       
004700*                                 ORDERKLASS                              
004800     03 DET-FLDIRLEV         PIC X.                                       
004900*                                 DIREKTLEVERANS ?                        
005000     03 DET-KDPRODSL         PIC Z9.                                      
005100*                                 PRODUKTSLAG                             
005200     03 DET-IDUSER-PACK      PIC X(8).                                    
005300*                                 ANSVARIGT USERID PACKARE                
005400     03 DET-IDFKNGRP         PIC Z(3)9.                                   
005500*                                 FUNKTIONSGRUPP                          
005600     03 DET-IDPRODNR         PIC Z(6)9.                                   
005700*                                 PRODUKTIONSNUMMER                       
005800     03 DET-TIINVDAT         PIC 9(5).                                    
005900*                                 INVENTERINGSDATUM                       
006000     03 DET-KVORDRAD         PIC Z(4)9.                                   
006100*                                 ANTAL ORDERRADER                        
006200     03 DET-KVINVS           PIC Z(6)9.                                   
006300*                                 INVENTERINGSSALDO                       
006400     03 DET-IDUSER-OREG      PIC X(8).                                    
006500*                                 ANSVARIGT USERID ORDERREG.              
006600     03 DET-IDANSK           PIC Z(2)9.                                   
006700*                                 ANSKAFFARNUMMER                         
006800     03 DET-TIREGDAT         PIC 9(6).                                    
006900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007000     03 DET-BEART            PIC X(25).                                   
007100*                                 ARTIKELBENÄMNING                        
007200     03 DET-PRARTBTO         PIC Z(6)9.9(2).                              
007300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
007400     03 DET-PRARTBTO-EXP     PIC Z(6)9.9(2).                              
007500*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
007600     03 DET-PRINK            PIC Z(6)9.9(2).                              
007700*                                 INKÖPSPRIS                              
007800     03 DET-TEANMNOT-REG     OCCURS 3 TIMES                               
007900                             PIC X(70).                                   
008000*                                 FRI TEXT FRÅN REGISTRERINGEN            
008100     03 DET-TEANMNOT-ADM     OCCURS 3 TIMES                               
008200                             PIC X(70).                                   
008300*                                 FRI TEXT FRÅN ADMINISTRATION            
008400     03 DET-TEANMNOT-REM     OCCURS 3 TIMES                               
008500                             PIC X(70).                                   
008600*                                 FRI TEXT FRÅN REMISSINSTANS             
008700     03 DET-TEANMNOT-RET     OCCURS 3 TIMES                               
008800                             PIC X(70).                                   
008900*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
009000*** END OF VILMAII-COPY LENGTH= 1075 BYTES                                
