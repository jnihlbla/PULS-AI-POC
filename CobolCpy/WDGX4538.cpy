000100 01  4538-WDGX4538.                                                       
000200*                                 PLANERAD PRODUKTION/TRP                 
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDTRPVAR, IDORDER, LOW-VALUE)          
000500     03 4538-IDTRPVAR        PIC X(2).                                    
000600*                                 TRANSPORTL÷SNINGSGRUPP                  
000700*                                 TRANSPORTSOLUTIONGROUP                  
000800     03 4538-IDORDER         PIC S9(7)           COMP-3.                  
000900*                                 VOLVO PARTS ORDERNUMMER                 
001000*                                 VOLVO PARTS ORDER NUMBER                
001100     03 4538-LOW-VALUE       PIC X(4).                                    
001200     03 4538-IDGMTREF.                                                    
001300*                                 GODSMOTTAGAREREFERENS                   
001400*                                 GOODS RECEIVER REFERENS                 
001500        05 4538-IDDISTR      PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800        05 4538-IDKUNDNR     PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000*                                 CUSTOMER NO                             
002100        05 4538-IDKUNDRF     PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300*                                 CUSTOMER REFERENCE (ORDER ID)           
002400        05 4538-IDORDNR5-FILLER REDEFINES 4538-IDKUNDRF.                  
002500           07 4538-IDORDNR5  PIC 9(5).                                    
002600*                                 ORDERNUMMER                             
002700*                                 ORDER NUMBER                            
002800           07 FILLER         PIC X(5).                                    
002900        05 4538-IDORDNR7-FILLER REDEFINES 4538-IDKUNDRF.                  
003000           07 4538-IDORDNR7  PIC 9(7).                                    
003100*                                 ORDERNUMMER                             
003200*                                 ORDER NUMBER                            
003300           07 FILLER         PIC X(3).                                    
003400     03 4538-TILST-O         PIC S9(11)          COMP-3.                  
003500*                                 SENASTE STARTTIDPUNKT F÷R ORDER         
003600*                                 LATEST START-TIME ORDER                 
003700     03 4538-TIRFS           PIC S9(11)          COMP-3.                  
003800*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
003900*                                 READY FOR SHIPMENT  YYMMDDHHMM          
004000     03 4538-TITRPAVT.                                                    
004100*                                 TRANSPORTAVG≈NGSTIDPUNKT                
004200*                                 TRANSPORT DEPARTURE                     
004300        05 4538-TIAAMMDD     PIC S9(7)           COMP-3.                  
004400*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
004500*                                 YEAR - MONTH - DAY  (YYMMDD)            
004600        05 4538-TIHHMM       PIC S9(5)           COMP-3.                  
004700*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004800*                                 TIME IN HOUR AND MINUTE                 
004900     03 4538-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
005000*                                 ORDERVIKT NETTO (KG)                    
005100*                                 WEIGHT PER ORDER NETTO (KG)             
005200     03 4538-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
005300*                                 ORDERVOLYM NETTO (M3)                   
005400*                                 NET VOLUME PER ORDER (M3)               
005500     03 4538-FILLER          PIC X(6).                                    
005600*** END COPY WDGX4538C0  LENGTH=60                                        
