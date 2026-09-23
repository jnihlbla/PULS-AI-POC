000100 01  SHUV-WDJ201.                                                         
000200*                                 SATSORDERREGISTER                       
000300*                                 SATSORDERHUVUD SEGMENT                  
000400*                                 FYSISK NYCKEL: IDORDNST                 
000500     03 SHUV-IDORDNST.                                                    
000600*                                 SATSORDERNUMMER-TOTALT                  
000700*                                 KIT-ORDER-NUMBER-TOTAL                  
000800        05 SHUV-IDORDNSB     PIC S9(5)           COMP-3.                  
000900*                                 SATSORDERNUMMER-BAS                     
001000*                                 KIT-ORDER-NUMBER-BASIC                  
001100        05 SHUV-IDORDNSS     PIC S9              COMP-3.                  
001200*                                 SATSORDERNUMMER-SUFFIX                  
001300*                                 KIT-ORDER-NUMBER-SUFFIX                 
001400     03 SHUV-FLBYGGB         PIC X.                                       
001500*                                 FLAGGA BYGGBAR SATSORDER                
001600*                                 FLAG POSSIBLE TO BUILD KIT-ORDE         
001700*                                 R                                       
001800     03 SHUV-FLSATNOL        PIC X.                                       
001900*                                 FLAGGA NOLLMARKERING SATS               
002000*                                 FLAG ZERO MARK  KIT                     
002100     03 SHUV-FLSATNYO        PIC X.                                       
002200*                                 FLAGGA NY SATSORDER                     
002300*                                 FLAG NEW KIT-ORDER                      
002400     03 SHUV-IDANALYS        PIC X(12).                                   
002500*                                 ANALYSNUMMER                            
002600*                                 ANALYSIS NUMBER                         
002700     03 SHUV-IDANSK          PIC S9(3)           COMP-3.                  
002800*                                 ANSKAFFARNUMMER                         
002900*                                 PROCURER NO.                            
003000     03 SHUV-IDARTNR         PIC S9(9)           COMP-3.                  
003100*                                 ARTIKELNUMMER                           
003200*                                 PART NUMBER                             
003300     03 SHUV-IDDISTR         PIC S9(5)           COMP-3.                  
003400*                                 DISTRIKTNUMMER                          
003500*                                 DISTRICT NUMBER                         
003600     03 SHUV-IDKONTO         PIC S9(11)          COMP-3.                  
003700*                                 KONTO                                   
003800*                                 ACCOUNT                                 
003900     03 SHUV-IDKST           PIC X(10).                                   
004000*                                 KOSTNADSSTÄLLE                          
004100*                                 COST CENTRE                             
004200     03 SHUV-IDKUNDNR        PIC S9(7)           COMP-3.                  
004300*                                 KUNDNUMMER                              
004400*                                 CUSTOMER NO                             
004500     03 SHUV-IDLEVNR         PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004800     03 SHUV-IDPRC.                                                       
004900*                                 PRODUKTIONSKANAL                        
005000*                                 PRODUCTION CHANNEL                      
005100        05 SHUV-IDPRCBAS     PIC X(3).                                    
005200*                                 PRC-BAS                                 
005300*                                 PRC-BASIC                               
005400        05 SHUV-IDPRCVAR     PIC X.                                       
005500*                                 PRC-VARIANT                             
005600*                                 PRC-VARIANT                             
005700     03 SHUV-IDPRODNR        PIC S9(7)           COMP-3.                  
005800*                                 PRODUKTIONSNUMMER                       
005900*                                 PRODUCTION NUMBER                       
006000     03 SHUV-IDUSER          PIC X(8).                                    
006100*                                 ANVÄNDARENS SÄKERHETS ID                
006200*                                 USER SECURITY-IDENTITY                  
006300     03 SHUV-KDCLAGER        PIC S9              COMP-3.                  
006400*                                 CENTRALLAGERKOD                         
006500*                                 CENTRAL WAREHOUSE CODE                  
006600     03 SHUV-KDFAKTYP        PIC X.                                       
006700*                                 FAKTURATYP                              
006800*                                 INVOICE TYPE                            
006900     03 SHUV-BEFT            PIC S9(3)           COMP-3.                  
007000*                                 FÖRPACKNINGSTYP                         
007100*                                 PACKAGING TYPE                          
007200     03 SHUV-FLSATSPR        PIC X.                                       
007300*                                 FLAGGA SPÄRRAD SATS EL SATSRAD          
007400*                                 FLAG ORDER OR LINE BLOCKED KIT          
007500     03 SHUV-KDORDKL         PIC S9              COMP-3.                  
007600*                                 ORDERKLASS                              
007700*                                 ORDER CLASS                             
007800     03 SHUV-KDPRODSL        PIC S9(3)           COMP-3.                  
007900*                                 PRODUKTSLAG                             
008000*                                 PRODUCT GROUP                           
008100     03 SHUV-KDSATKMB        PIC X.                                       
008200*                                 KOMBINATIONSKOD SATS                    
008300*                                 MATCHING CODE KIT                       
008400     03 SHUV-KDSATPLK        PIC X.                                       
008500*                                 PLOCKSATSSTATUS SATSORDER  KDSA         
008600*                                 TPLK                                    
008700*                                 PICKING UNIT STATUS KIT-ORDER K         
008800*                                 DSATPLK                                 
008900     03 SHUV-KDSATSTA        PIC X.                                       
009000*                                 STATUS FÖR SATSORDER                    
009100*                                 STATUS OF KIT ORDER                     
009200     03 SHUV-KVBEART         PIC S9(7)           COMP-3.                  
009300*                                 BESTÄLLT ANTAL STYCKEN                  
009400*                                 ORDERED QUANTITY                        
009500     03 SHUV-KVBYGGB         PIC S9(7)           COMP-3.                  
009600*                                 ANTAL BYGGBARA SATSER                   
009700*                                 QTY OF KITS POSSIBLE TO BUILD           
009800     03 SHUV-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
009900*                                 ARTIKELSTANDARDPRIS                     
010000*                                 STANDARD PRICE                          
010100     03 SHUV-REKSIFFR        PIC S9              COMP-3.                  
010200*                                 KONTROLLSIFFRA                          
010300*                                 PART NO CHECK DIGIT                     
010400     03 SHUV-SUSATPTI        PIC S9(3)V9(2)      COMP-3.                  
010500*                                 TOT PRODUKTIONSTID SATS TIM MIN         
010600*                                 TOT PRODUCTIONTIME KIT HOUR MIN         
010700     03 SHUV-TIBEGPAC        PIC S9(7)           COMP-3.                  
010800*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
010900*                                 REQUESTED PACKING DATE (YYMMDD)         
011000     03 SHUV-DAREGDAT        PIC 9(8).                                    
011100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
011200*                                 REGISTRATION DATE (YYYYMMDD)            
011300     03 SHUV-TIUPPDAT        PIC S9(7)           COMP-3.                  
011400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
011500*                                 UPDATING DATE     (YYMMDD)              
011600     03 SHUV-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
011700*                                 ORDERVIKT NETTO (KG)                    
011800*                                 WEIGHT PER ORDER NETTO (KG)             
011900     03 SHUV-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
012000*                                 ORDERVOLYM NETTO (M3)                   
012100*                                 NET VOLUME PER ORDER (M3)               
012200     03 SHUV-KVRORAD-9KOMPL  PIC S9(5)           COMP-3.                  
012300*                                 ANTAL RESTORDER-RADER 9-KOMPL           
012400*                                 NBR OF BACK ORDER ITEMS 9-COMPL         
012500     03 SHUV-RELSKVOT-L      PIC S9(3)V9(2)      COMP-3.                  
012600*                                 LAGERSALDO/PERIODBEH LITEN SATS         
012700*                                 STOCK BALANCE/PERIOD RQ SM KIT          
012800     03 SHUV-FLSATPRI        PIC X.                                       
012900*                                 MANUELL PRIORITERING AV SATS            
013000*                                 MANUAL PRIORITY OF KIT-ORDER            
013100     03 SHUV-RELSKVOT-S      PIC S9(3)V9(2)      COMP-3.                  
013200*                                 LAGERSALDO/PERIODBEH STOR  SATS         
013300*                                 STOCK BALANCE/PERIOD RQ GT KIT          
013400     03 SHUV-FILLER          PIC X(8).                                    
013500*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 
