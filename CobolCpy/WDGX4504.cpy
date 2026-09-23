000100 01  4504-WDGX4504.                                                       
000200*                                 KOLLI INFO TILL HIT-TRANSPORT÷R         
000300*                                 WDR4                                    
000400*                                 FYSISK NYCKEL KY4504:                   
000500*                                 (IDPRODNR + IDKOLLI)                    
000600     03 4504-IDPRODNR        PIC S9(7)           COMP-3.                  
000700*                                 PRODUKTIONSNUMMER                       
000800*                                 PRODUCTION NUMBER                       
000900     03 4504-IDKOLLI         PIC S9(5)           COMP-3.                  
001000*                                 KOLLINUMMER                             
001100*                                 CASE NUMBER                             
001200     03 4504-ADGMT-PADR      PIC X(35).                                   
001300*                                 GODSMOTTAGARADRESS POSTADRESS           
001400*                                 GOODS RECEIVER ADDRESS TOWN             
001500     03 4504-ADFLGEO         PIC X(3).                                    
001600*                                 GEOGRAFISKT OMR≈DE FƒRDIGLAGER          
001700*                                 GEOGRAPHIC AREA                         
001800     03 4504-IDGMTREF.                                                    
001900*                                 GODSMOTTAGAREREFERENS                   
002000*                                 GOODS RECEIVER REFERENS                 
002100        05 4504-IDDISTR      PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300*                                 DISTRICT NUMBER                         
002400        05 4504-IDKUNDNR     PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700        05 4504-IDKUNDRF-GRP.                                             
002800*                                 KUNDENS REFERENS (ORDERID)              
002900*                                 CUSTOMER REFERENCE (ORDER ID)           
003000           07 4504-IDKUNDRF  PIC X(10).                                   
003100*                                 KUNDENS REFERENS (ORDERID)              
003200*                                 CUSTOMER REFERENCE (ORDER ID)           
003300           07 4504-IDORDNR5-FILLER REDEFINES 4504-IDKUNDRF.               
003400              09 4504-IDORDNR5                                            
003500                             PIC 9(5).                                    
003600*                                 ORDERNUMMER                             
003700*                                 ORDER NUMBER                            
003800              09 FILLER      PIC X(5).                                    
003900           07 4504-IDORDNR7-FILLER REDEFINES 4504-IDKUNDRF.               
004000              09 4504-IDORDNR7                                            
004100                             PIC 9(7).                                    
004200*                                 ORDERNUMMER                             
004300*                                 ORDER NUMBER                            
004400              09 FILLER      PIC X(3).                                    
004500     03 4504-IDGODS          PIC 9(2).                                    
004600*                                 GODSTYP P≈ TRANSPORTDOKUMENT            
004700*                                 GOODS TYPE ON FREIGHT REPORTS           
004800     03 4504-IDKLIID         PIC 9(11).                                   
004900*                                 KOLLIID NUMMER TILL HIT                 
005000*                                 CASE ID. NUMBER FOR HIT                 
005100     03 4504-DAREGDAT        PIC 9(8).                                    
005200*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
005300*                                 REGISTRATION DATE (YYYYMMDD)            
005400     03 4504-TIHHMM          PIC S9(5)           COMP-3.                  
005500*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005600*                                 TIME IN HOUR AND MINUTE                 
005700     03 4504-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
005800*                                 ORDERVIKT BRUTTO PER KOLLI              
005900*                                 ORDER WEIGHT GROSS PER CASE             
006000     03 4504-VLORDBTO-KOLLI  PIC S9(4)V9(3)      COMP-3.                  
006100*                                 ORDERVOLYM BRUTTO KOLLI                 
006200*                                 ORDER VOL GR/CASE                       
006300     03 4504-FILLER          PIC X(6).                                    
006400*** END OF VILMAII-COPY LENGTH= 100 BYTES                                 
