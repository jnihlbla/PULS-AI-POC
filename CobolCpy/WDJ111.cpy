000100 01  RAD-WDJ111.                                                          
000200*                                 SATSSTRUKTUWREGISTER                    
000300*                                 STRUKTURRAD SEGMENT                     
000400*                                 FYSISK NYCKEL: WDJ111KY                 
000500*                                 (KDSTRRAD + IDRADNR)                    
000600*                                 SÖKBEGREPP: TISTADAT, TISTODAT          
000700     03 RAD-KDSTRRAD         PIC X.                                       
000800*                                 TYP AV STRUKTURRAD                      
000900*                                 TYPE OF LINE IN A STRUCTURE             
001000     03 RAD-IDRADNR          PIC S9(5)           COMP-3.                  
001100*                                 RADNUMMER                               
001200*                                 LINE NO                                 
001300     03 RAD-IDLEVNR          PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001600     03 RAD-BELEVART         PIC X(30).                                   
001700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001800*                                 SUPPLIERS PART DESCRIPTION              
001900     03 RAD-IDARTNR          PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 RAD-BEART-SVE        PIC X(25).                                   
002300*                                 SVENSK ARTIKELBENÄMNING                 
002400     03 RAD-IDAO-STA         PIC X(10).                                   
002500*                                 ÄNDRINGSORDERNUMMER START               
002600*                                 DESIGN CHANGE NOTICE START              
002700     03 RAD-IDAO-STO         PIC X(10).                                   
002800*                                 ÄNDRINGSORDERNUMMER STOP                
002900*                                 DESIGN CHANGE NOTICE STOP               
003000     03 RAD-IDSTRTYP         PIC X.                                       
003100*                                 STRUKTURTYP                             
003200*                                 TYPE OF STRUCTURE                       
003300     03 RAD-KDBENHOM         PIC S9              COMP-3.                  
003400*                                 HOMONYMKOD                              
003500*                                 HOMONYMOUS CODE                         
003600     03 RAD-KDISATS          PIC X.                                       
003700*                                 STATUSKOD I SATS                        
003800     03 RAD-KDSORT           PIC X(2).                                    
003900*                                 SORT-KOD                                
004000*                                 UNIT OF MEASURE                         
004100     03 RAD-REANTPSA         PIC S9(2)V9(3)      COMP-3.                  
004200*                                 ANTAL PER SATS                          
004300     03 RAD-TIREGDAT         PIC S9(7)           COMP-3.                  
004400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004500*                                 REGISTRATION DATE (YYMMDD)              
004600     03 RAD-TISTADAT         PIC S9(7)           COMP-3.                  
004700*                                 GENERELLT STARTDATUM                    
004800*                                 GENERAL START DATE                      
004900     03 RAD-TISTODAT         PIC S9(7)           COMP-3.                  
005000*                                 GENERELLT STOPPDATUM                    
005100*                                 GENERAL STOP DATE YYMMDD                
005200     03 FILLER               PIC X(11).                                   
005300*** END OF VILMAII-COPY LENGTH= 120 BYTES                                 
