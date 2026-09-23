000100 01  EKH-W510EKHA.                                                        
000200*                                 EKH                                     
000300*                                 SKAPAS FÖR EKONOMISK                    
000400*                                 HUVUDHÄNDELSE.                          
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 EKH-BEVAT            PIC X(2).                                    
000800*                                 MOMSKODSBENÄMNING                       
000900     03 EKH-DAVERDAT         PIC 9(8).                                    
001000*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
001100     03 EKH-FLLSBOK          PIC X.                                       
001200*                                 LAGERAVBOKNING                          
001300     03 EKH-IDANALYS         PIC X(12).                                   
001400*                                 ANALYSNUMMER                            
001500     03 EKH-IDARTNR          PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 EKH-IDDC-SEND        PIC X(2).                                    
001800*                                 SÄNDANDE LAGER                          
001900     03 EKH-IDDC-REC         PIC X(2).                                    
002000*                                 MOTTAGANDE LAGER                        
002100     03 EKH-IDDISTR          PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300     03 EKH-IDKONTO          PIC S9(11)          COMP-3.                  
002400*                                 KONTO                                   
002500     03 EKH-IDKST            PIC X(10).                                   
002600*                                 KOSTNADSSTÄLLE                          
002700     03 EKH-IDKUNDNR         PIC S9(7)           COMP-3.                  
002800*                                 KUNDNUMMER                              
002900     03 EKH-IDTRANS          PIC X(4).                                    
003000*                                 BILDNUMMER                              
003100     03 EKH-IDVERGL          PIC X(10).                                   
003200*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
003300     03 EKH-KDANMORS         PIC X(2).                                    
003400*                                 ORSAK TILL LEVERANSANMÄRKNING           
003500     03 EKH-KDEKHHT          PIC X(3).                                    
003600*                                 EKONOMISK HUVUDHÄNDELSE                 
003700     03 EKH-KDEKSHT          PIC X(3).                                    
003800*                                 EKONOMISK SUBHÄNDELSE                   
003900     03 EKH-KDEKNIVA         PIC X(5).                                    
004000*                                 EKONOMISK HÄNDELSENIVÅ                  
004100     03 EKH-KDFRAKT          PIC S9(3)           COMP-3.                  
004200*                                 FRAKTSÄTT DC TILL KUND                  
004300     03 EKH-KDPRODSL         PIC S9(3)           COMP-3.                  
004400*                                 PRODUKTSLAG                             
004500     03 EKH-KDPSLLOC         PIC 9(2).                                    
004600*                                 PRODUKTSLAG LOKALT                      
004700     03 EKH-KDVALISO         PIC X(3).                                    
004800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004900     03 EKH-KVANTAL          PIC S9(7)           COMP-3.                  
005000*                                 ANTAL                                   
005100     03 EKH-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
005200*                                 ARTIKELPRIS NETTO                       
005300     03 EKH-PRARTSJK         PIC S9(7)V9(2)      COMP-3.                  
005400*                                 ARTIKELNS SJÄLVKOSTNAD                  
005500     03 EKH-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
005600*                                 ARTIKELSTANDARDPRIS                     
005700     03 EKH-PRDIRLON         PIC S9(4)V9(3)      COMP-3.                  
005800*                                 DIREKT LÖN                              
005900     03 EKH-PRDMTRL          PIC S9(6)V9(3)      COMP-3.                  
006000*                                 DIREKT MATERIAL                         
006100     03 EKH-PRINK            PIC S9(7)V9(2)      COMP-3.                  
006200*                                 INKÖPSPRIS                              
006300     03 EKH-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
006400*                                 VALUTAKURS                              
006500     03 EKH-PRLANDCO         PIC S9(7)V9(2)      COMP-3.                  
006600*                                 LANDING COST                            
006700     03 EKH-PROVRPAL         PIC S9(4)V9(3)      COMP-3.                  
006800*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
006900     03 EKH-SUBEL            PIC S9(9)V9(2).                              
007000*                                 SUMMABELOPP                             
007100     03 EKH-SUVAT            PIC S9(11)V9(2)     COMP-3.                  
007200*                                 MOMSVÄRDE PER MOMSKOD                   
007300     03 EKH-DAAVIDAT         PIC 9(8).                                    
007400*                                 AVISERINGSDATUM (YYYYMMDD)              
007500     03 EKH-IDAVINR          PIC S9(7)           COMP-3.                  
007600*                                 AVI-NUMMER                              
007700     03 EKH-IDLEVNR          PIC X(5).                                    
007800*                                 LEVERANTÖRNUMMER                        
007900     03 EKH-KDAVVTYP         PIC S9              COMP-3.                  
008000*                                 AVVIKELSETYP                            
008100*                                 1=POSITIV.  2=NEGATIV                   
008200     03 EKH-KDRT             PIC S9(3)           COMP-3.                  
008300*                                 REDOVISNINGSTYP                         
008400     03 EKH-KVANTMOT         PIC S9(7)           COMP-3.                  
008500*                                 ANTAL MOTTAGET                          
008600     03 EKH-KVAVIS           PIC S9(7)           COMP-3.                  
008700*                                 AVISERAT ANTAL                          
008800     03 EKH-KDSORT           PIC X(2).                                    
008900*                                 SORT-KOD                                
009000     03 EKH-KDTRADP          PIC X(4).                                    
009100*                                 TRADING PARTNER                         
009200     03 EKH-FLOVRLEV         PIC X.                                       
009300*                                 ÖVERLEVERANS                            
009400     03 EKH-IDORDNR5         PIC S9(5)           COMP-3.                  
009500*                                 ORDERNUMMER                             
009600     03 EKH-IDUSER           PIC X(8).                                    
009700*                                 ANVÄNDARENS SÄKERHETS ID                
009800     03 EKH-IDREF            PIC X(15).                                   
009900*                                 REFERENS ID                             
010000     03 EKH-BEFELSAP         PIC X(20).                                   
010100*                                 FELTEXT FÖR SAP-TRANSAKTIONER           
010200     03 EKH-FLKLAR           PIC X.                                       
010300*                                 AVSLUTNINGSMARKERING                    
010400     03 EKH-PRHEMTAG         PIC S9(7)V9(2)      COMP-3.                  
010500*                                 HEMTAGNINGSKOSTNAD                      
010600     03 EKH-FLDCET           PIC X.                                       
010700*                                 DC 91 EXCHANGE TERMINAL                 
010800     03 EKH-IDKUNDRF         PIC X(10).                                   
010900*                                 KUNDENS REFERENS (ORDERID)              
011000     03 EKH-IDFAKT-EXP       PIC X(7).                                    
011100*                                 FAKTNR NR.1 I EXPORTFLÖDET              
011200     03 EKH-CMD              PIC X(3).                                    
011300*** END OF VILMAII-COPY LENGTH= 265 BYTES                                 
