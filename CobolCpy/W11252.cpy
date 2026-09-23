000100 01  W11252.                                                              
000200*                                 COPYTEXT TILL FIL W11250                
000300*                                 HISTORIK STRUKTURER                     
000400*                                 INNEHÅLLER STRUKTURENS                  
000500*                                 RADER                                   
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 KDSTRRAD             PIC X.                                       
000900*                                 TYP AV STRUKTURRAD                      
001000     03 IDRADNR              PIC S9(5)           COMP-3.                  
001100*                                 RADNUMMER                               
001200     03 IDLEVNR              PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 BELEVART             PIC X(30).                                   
001500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 BEART-SVE            PIC X(25).                                   
001900*                                 SVENSK ARTIKELBENÄMNING                 
002000     03 IDAO-STA             PIC X(10).                                   
002100*                                 ÄNDRINGSORDERNUMMER START               
002200     03 IDAO-STO             PIC X(10).                                   
002300*                                 ÄNDRINGSORDERNUMMER STOP                
002400     03 IDSTRTYP             PIC X.                                       
002500*                                 STRUKTURTYP                             
002600     03 KDBENHOM             PIC S9              COMP-3.                  
002700*                                 HOMONYMKOD                              
002800     03 KDISATS              PIC X.                                       
002900*                                 STATUSKOD I SATS                        
003000     03 KDSORT               PIC X(2).                                    
003100*                                 SORT-KOD                                
003200     03 REANTPSA             PIC S9(2)V9(3)      COMP-3.                  
003300*                                 ANTAL PER SATS                          
003400     03 TIREGDAT             PIC S9(7)           COMP-3.                  
003500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003600     03 TISTADAT             PIC S9(7)           COMP-3.                  
003700*                                 GENERELLT STARTDATUM                    
003800     03 TISTODAT             PIC S9(7)           COMP-3.                  
003900*                                 GENERELLT STOPPDATUM                    
004000*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 
