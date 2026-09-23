000100 01  W41303-CTX.                                                          
000200*                                 WOPS - OUTPUT FROM W41303-PGM           
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 KDPRCGRP             PIC X(5).                                    
001000*                                 PRODUKTIONSKANALSGRUPP                  
001100*                                 GROUP OF PRODUCTION CHANNELS            
001200     03 IDPRC.                                                            
001300*                                 PRODUKTIONSKANAL                        
001400*                                 PRODUCTION CHANNEL                      
001500        05 IDPRCBAS          PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700*                                 PRC-BASIC                               
001800        05 IDPRCVAR          PIC X.                                       
001900*                                 PRC-VARIANT                             
002000*                                 PRC-VARIANT                             
002100     03 IDTRP.                                                            
002200*                                 TRANSPORTIDENTITET                      
002300*                                 TRANSPORTIDENTITY                       
002400        05 IDTRPLOS          PIC X(3).                                    
002500*                                 TRANSPORTL÷SNING                        
002600*                                 TRANSPORTSOLUTION                       
002700        05 IDTRPVAR          PIC X(2).                                    
002800*                                 TRANSPORTL÷SNINGSGRUPP                  
002900*                                 TRANSPORTSOLUTIONGROUP                  
003000     03 IDGMTREF.                                                         
003100*                                 GODSMOTTAGAREREFERENS                   
003200*                                 GOODS RECEIVER REFERENS                 
003300        05 IDDISTR           PIC S9(5)           COMP-3.                  
003400*                                 DISTRIKTNUMMER                          
003500*                                 DISTRICT NUMBER                         
003600        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
003700*                                 KUNDNUMMER                              
003800*                                 CUSTOMER NO                             
003900        05 IDKUNDRF-GRP.                                                  
004000*                                 KUNDENS REFERENS (ORDERID)              
004100*                                 CUSTOMER REFERENCE (ORDER ID)           
004200           07 IDKUNDRF       PIC X(10).                                   
004300*                                 KUNDENS REFERENS (ORDERID)              
004400*                                 CUSTOMER REFERENCE (ORDER ID)           
004500           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
004600              09 IDORDNR5    PIC 9(5).                                    
004700*                                 ORDERNUMMER                             
004800*                                 ORDER NUMBER                            
004900              09 FILLER      PIC X(5).                                    
005000           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
005100              09 IDORDNR7    PIC 9(7).                                    
005200*                                 ORDERNUMMER                             
005300*                                 ORDER NUMBER                            
005400              09 FILLER      PIC X(3).                                    
005500     03 TIRFS                PIC S9(11)          COMP-3.                  
005600*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
005700*                                 READY FOR SHIPMENT  YYMMDDHHMM          
005800     03 DALSTORD             PIC 9(12).                                   
005900*                                 SENASTE STARTTIDPUNKT F÷R ORDER         
006000*                                 LATEST START-TIME ORDER                 
006100     03 IDORDER              PIC S9(7)           COMP-3.                  
006200*                                 VOLVO PARTS ORDERNUMMER                 
006300*                                 VOLVO PARTS ORDER NUMBER                
006400     03 KDODELSTA            PIC X.                                       
006500*                                 ORDERDELSTATUS                          
006600*                                 ORDER PART STATUS                       
006700     03 KDPRODKL             PIC X.                                       
006800*                                 PRODUKTIONSKLASS                        
006900*                                 PRODUCTION CLASS                        
007000     03 KVRADER              PIC S9(5)           COMP-3.                  
007100*                                 ANTAL RADER                             
007200*                                 NUMBER OF LINES                         
007300     03 SUPTID               PIC S9(3)V9(2)      COMP-3.                  
007400*                                 TOTAL PRODUKTIONSTID TIM+MIN            
007500*                                 TOTAL PRODUCTIONTIME HOUR MIN.          
007600     03 VKORDNTO             PIC S9(6)V9(1)      COMP-3.                  
007700*                                 ORDERVIKT NETTO (KG)                    
007800*                                 WEIGHT PER ORDER NETTO (KG)             
007900     03 VLORDNTO             PIC S9(4)V9(3)      COMP-3.                  
008000*                                 ORDERVOLYM NETTO (M3)                   
008100*                                 NET VOLUME PER ORDER (M3)               
008200     03 TITRPAVT.                                                         
008300*                                 TRANSPORTAVG≈NGSTIDPUNKT                
008400*                                 TRANSPORT DEPARTURE                     
008500        05 TIAAMMDD          PIC S9(7)           COMP-3.                  
008600*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
008700*                                 YEAR - MONTH - DAY  (YYMMDD)            
008800        05 TIHHMM            PIC S9(5)           COMP-3.                  
008900*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
009000*                                 TIME IN HOUR AND MINUTE                 
009100     03 IDPRODNR             PIC S9(7)           COMP-3.                  
009200*                                 PRODUKTIONSNUMMER                       
009300*                                 PRODUCTION NUMBER                       
009400     03 KVPTID               PIC S9(2)V9(1)      COMP-3.                  
009500*                                 GENOMSNITTLIG TID/RAD MINUTER           
009600*                                 AVERIDGE TIME/LINE MINUTES              
009700     03 KDFDKRAV             PIC S9(3)           COMP-3.                  
009800*                                 TRANSPORTF÷RPACKNINGSKOD                
009900*                                 PACKING CODE                            
010000     03 TIREGDAT             PIC S9(7)           COMP-3.                  
010100*                                 REGISTRERINGSDATUM (≈≈MMDD)             
010200*                                 REGISTRATION DATE (YYMMDD)              
010300     03 TIPACKN              PIC S9(7)           COMP-3.                  
010400*                                 PACKNINGSDATUM         (≈≈MMDD)         
010500*                                 PACKING DATE           (YYMMDD)         
010600*** END OF VILMAII-COPY LENGTH= 97 BYTES                                  
