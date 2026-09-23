000100 01  ODEL-WDQ301.                                                         
000200*                                 ORDERDELSREGISTER KÖ                    
000300*                                 FYSISK NYCKEL: WDQ301KY                 
000400*                                 IDORDER  + IDDC +                       
000500*                                 IDPRODNR + IDPLKLST                     
000600     03 ODEL-IDORDER         PIC S9(7)           COMP-3.                  
000700*                                 VOLVO PARTS ORDERNUMMER                 
000800*                                 VOLVO PARTS ORDER NUMBER                
000900     03 ODEL-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 ODEL-IDPRODNR        PIC S9(7)           COMP-3.                  
001300*                                 PRODUKTIONSNUMMER                       
001400*                                 PRODUCTION NUMBER                       
001500     03 ODEL-IDPLKLST        PIC S9(3)           COMP-3.                  
001600*                                 PLOCKLISTNUMMER                         
001700*                                 PICKING LIST NUMBER                     
001800     03 ODEL-IDBORD          PIC X(3).                                    
001900*                                 PACK-BORD                               
002000*                                 PACKING TABLE                           
002100     03 ODEL-IDGMTREF.                                                    
002200*                                 GODSMOTTAGAREREFERENS                   
002300*                                 GOODS RECEIVER REFERENS                 
002400        05 ODEL-IDDISTR      PIC S9(5)           COMP-3.                  
002500*                                 DISTRIKTNUMMER                          
002600*                                 DISTRICT NUMBER                         
002700        05 ODEL-IDKUNDNR     PIC S9(7)           COMP-3.                  
002800*                                 KUNDNUMMER                              
002900*                                 CUSTOMER NO                             
003000        05 ODEL-IDKUNDRF-GRP.                                             
003100*                                 KUNDENS REFERENS (ORDERID)              
003200*                                 CUSTOMER REFERENCE (ORDER ID)           
003300           07 ODEL-IDKUNDRF  PIC X(10).                                   
003400*                                 KUNDENS REFERENS (ORDERID)              
003500*                                 CUSTOMER REFERENCE (ORDER ID)           
003600           07 ODEL-IDORDNR5-FILLER REDEFINES ODEL-IDKUNDRF.               
003700              09 ODEL-IDORDNR5                                            
003800                             PIC 9(5).                                    
003900*                                 ORDERNUMMER                             
004000*                                 ORDER NUMBER                            
004100              09 FILLER      PIC X(5).                                    
004200           07 ODEL-IDORDNR7-FILLER REDEFINES ODEL-IDKUNDRF.               
004300              09 ODEL-IDORDNR7                                            
004400                             PIC 9(7).                                    
004500*                                 ORDERNUMMER                             
004600*                                 ORDER NUMBER                            
004700              09 FILLER      PIC X(3).                                    
004800     03 ODEL-IDLEVNR         PIC X(5).                                    
004900*                                 LEVERANTÖRNUMMER                        
005000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005100     03 ODEL-IDPRC.                                                       
005200*                                 PRODUKTIONSKANAL                        
005300*                                 PRODUCTION CHANNEL                      
005400        05 ODEL-IDPRCBAS     PIC X(3).                                    
005500*                                 PRC-BAS                                 
005600*                                 PRC-BASIC                               
005700        05 ODEL-IDPRCVAR     PIC X.                                       
005800*                                 PRC-VARIANT                             
005900*                                 PRC-VARIANT                             
006000     03 ODEL-IDUSER          PIC X(8).                                    
006100*                                 ANVÄNDARENS SÄKERHETS ID                
006200*                                 USER SECURITY-IDENTITY                  
006300     03 ODEL-KDFDKRAV        PIC S9(3)           COMP-3.                  
006400*                                 TRANSPORTFÖRPACKNINGSKOD                
006500*                                 PACKING CODE                            
006600     03 ODEL-KDODELSTA       PIC X.                                       
006700*                                 ORDERDELSTATUS                          
006800*                                 ORDER PART STATUS                       
006900     03 ODEL-KDPRODKL        PIC X.                                       
007000*                                 PRODUKTIONSKLASS                        
007100*                                 PRODUCTION CLASS                        
007200     03 ODEL-KVART           PIC S9(7)           COMP-3.                  
007300*                                 ANTAL ARTNR PER BRYTBEGREPP             
007400*                                 NO OF PARTNOS PER TYPE                  
007500     03 ODEL-KVPACKRAD-OD    PIC S9(5)           COMP-3.                  
007600*                                 ANTAL PACKADE RADER                     
007700*                                 NUMBER OF PACKED RADER                  
007800     03 ODEL-KVPTID          PIC S9(2)V9(1)      COMP-3.                  
007900*                                 GENOMSNITTLIG TID/RAD MINUTER           
008000*                                 AVERIDGE TIME/LINE MINUTES              
008100     03 ODEL-KVRADER         PIC S9(5)           COMP-3.                  
008200*                                 ANTAL RADER                             
008300*                                 NUMBER OF LINES                         
008400     03 ODEL-SUHANTTI        PIC S9(7)           COMP-3.                  
008500*                                 SUMMA HANTERINGSKOD TID                 
008600*                                 TOTAL PIECEWORK TIME                    
008700     03 ODEL-SUORDV          PIC S9(9)V9(2)      COMP-3.                  
008800*                                 SUMMA ORDERVÄRDE                        
008900*                                 TOTAL ORDER VALUE                       
009000     03 ODEL-SUPTID          PIC S9(3)V9(2)      COMP-3.                  
009100*                                 TOTAL PRODUKTIONSTID TIM+MIN            
009200*                                 TOTAL PRODUCTIONTIME HOUR MIN.          
009300     03 ODEL-TILST-OD        PIC S9(11)          COMP-3.                  
009400*                                 SENASTE STARTTIDPUNKT ORDERDEL          
009500*                                 LATEST START-TIME ORDER-PART            
009600     03 ODEL-TIPACKN         PIC S9(7)           COMP-3.                  
009700*                                 PACKNINGSDATUM         (ÅÅMMDD)         
009800*                                 PACKING DATE           (YYMMDD)         
009900     03 ODEL-TIPACTID        PIC S9(7)           COMP-3.                  
010000*                                 PACKNINGSTID  TTMMSS                    
010100*                                 PACKING TIME  HHMMSS                    
010200     03 ODEL-TIREGDAT        PIC S9(7)           COMP-3.                  
010300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010400*                                 REGISTRATION DATE (YYMMDD)              
010500     03 ODEL-TIREGTID        PIC S9(7)           COMP-3.                  
010600*                                 REGISTRERINGSTID                        
010700*                                 GENERAL REGISTRATION TIME               
010800     03 ODEL-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
010900*                                 ORDERVIKT NETTO (KG)                    
011000*                                 WEIGHT PER ORDER NETTO (KG)             
011100     03 ODEL-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
011200*                                 ORDERVOLYM NETTO (M3)                   
011300*                                 NET VOLUME PER ORDER (M3)               
011400     03 ODEL-IDTRP.                                                       
011500*                                 TRANSPORTIDENTITET                      
011600*                                 TRANSPORTIDENTITY                       
011700        05 ODEL-IDTRPLOS     PIC X(3).                                    
011800*                                 TRANSPORTLÖSNING                        
011900*                                 TRANSPORTSOLUTION                       
012000        05 ODEL-IDTRPVAR     PIC X(2).                                    
012100*                                 TRANSPORTLÖSNINGSGRUPP                  
012200*                                 TRANSPORTSOLUTIONGROUP                  
012300     03 ODEL-DATRPAVT.                                                    
012400*                                 TRANSPORTAVGÅNGSTIDPUNKT                
012500*                                 TRANSPORT DEPARTURE                     
012600*                                 YYYYMMDD+HHMM                           
012700        05 ODEL-DATRPAVD     PIC 9(8).                                    
012800*                                 TRANSPORTAVGÅNGSDATUM                   
012900*                                 TRANSPORT DEPARTURE DATE                
013000        05 ODEL-TIHHMM       PIC S9(5)           COMP-3.                  
013100*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
013200*                                 TIME IN HOUR AND MINUTE                 
013300     03 ODEL-DAUTSKR         PIC 9(8).                                    
013400*                                 UTSKRIFTDATUM  (ÅÅÅÅMMDD)               
013500*                                 PRINTING DATE  (CCYYMMDD)               
013600     03 ODEL-TIUTSTID        PIC S9(7)           COMP-3.                  
013700*                                 UTSKRIFTSTID (TTMMSS)                   
013800*                                 TIME OF PRINTING (HHMMSS)               
013900     03 ODEL-DARFS           PIC 9(12).                                   
014000*                                 KLART FÖR TRANSPORT                     
014100*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
014200     03 ODEL-DALSTORD        PIC 9(12).                                   
014300*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
014400*                                 LATEST START-TIME ORDER                 
014500     03 ODEL-DARFSDAT        PIC 9(8).                                    
014600*                                 KLART FÖR TRANSPORT ÅÅÅÅMMDD            
014700*                                 READY FOR SHIPMENT  YYYYMMDD            
014800     03 ODEL-DEAL-PR-SUM.                                                 
014900*                                 DEALERPRIS (HUVUD)                      
015000        05 ODEL-SUORDV-LOC   PIC S9(9)V9(2)      COMP-3.                  
015100*                                 ORDERVÄRDE SLUTKUNDPRIS                 
015200*                                 I LOKAL VALUTA                          
015300*                                 ORDER VALUE, CUSTOMER PRICE             
015400*                                 IN LOCAL CURRENCY                       
015500        05 ODEL-SUORDV-LOCPREL                                            
015600                             PIC S9(9)V9(2)      COMP-3.                  
015700*                                 ORDERVÄRDE PREL SLUT-                   
015800*                                 KUNDPRIS, LOKAL VALUTA                  
015900*                                 ORDER VALUE, PREL CUSTOMER              
016000*                                 PRICE IN LOCAL CURRENCY                 
016100        05 ODEL-KDVALISO     PIC X(3).                                    
016200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
016300*                                 CURRENCY CODE BY ISO-STANDARD.          
016400     03 ODEL-IDPRCPLK        PIC X(4).                                    
016500*                                 ID FÖR EN PLOCKRUNDA                    
016600*                                 ID FOR A PICKING UNIT                   
016700     03 ODEL-IDLOTNR-PLK     PIC S9(3)           COMP-3.                  
016800*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
016900*                                 ORDERLOT NUMBER FOR A PICKLIST          
017000     03 ODEL-IDDC-EXP        PIC X(2).                                    
017100*                                 DC FÖR STUDS FLÖDE VID EXPORT           
017200*                                 DC FOR BOUNCE FLOW WHEN EXPORT          
017300     03 ODEL-IDLOPNR-ORD     PIC S9(3)           COMP-3.                  
017400*                                 ORDERNS ORDNINGSNUMMER INOM             
017500*                                 EN PLOCKSATS                            
017600*                                 SEQUENCE-NUMBER FOR AN ORDER            
017700*                                 WITHIN A PICKING UNIT                   
017800     03 ODEL-FILLER          PIC X(7).                                    
017900*** END OF VILMAII-COPY LENGTH= 200 BYTES                                 
