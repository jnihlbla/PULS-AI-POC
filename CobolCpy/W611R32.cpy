000100 01  W611R32.                                                             
000200*                                 LOGGPOST                                
000300*                                 POSTTYP R32                             
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 KDSORT2              PIC S9(3)           COMP-3.                  
000800*                                 SORTERINGSFÄLT                          
000900*                                 FIELD FOR SORTING PURPOSE               
001000     03 KDCLAGER             PIC S9              COMP-3.                  
001100      88 KDCLAGER-C1         VALUE +1.                                    
001200      88 KDCLAGER-C2         VALUE +2.                                    
001300*                                 CENTRALLAGERKOD                         
001400*                                 CENTRAL WAREHOUSE CODE                  
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700*                                 PART NUMBER                             
001800     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002000*                                 (0VVDLLLLK)                             
002100*                                 SERIAL NO RECEIVING REPORT              
002200*                                 (0WWDLLLLC)                             
002300     03 KDAVVANT             PIC S9              COMP-3.                  
002400*                                 AVVIKELSEANTAL KOD                      
002500*                                 0=INGEN ANM.   1=AVVIKELSE              
002600*                                 2=MAKULERING AV MOTT.RAPPORT            
002700*                                 QUANTITY DEVIATION  CODE                
002800*                                 0=NO DEV.    1=DEVIATION                
002900*                                 2=CANCELLING OF REC. REPORT             
003000     03 KVANTMOT             PIC S9(7)           COMP-3.                  
003100*                                 ANTAL MOTTAGET                          
003200*                                 QUANTITY RECEIVED                       
003300     03 KVFORDEL             PIC S9(7)           COMP-3.                  
003400*                                 ANTAL FÖRDELAT                          
003500*                                 SPLIT QUANTITY                          
003600     03 IDKOLLI              PIC S9(7)           COMP-3.                  
003700*                                 KOLLINUMMER         IDKOLLI-002         
003800     03 KDAVVKV              PIC S9              COMP-3.                  
003900*                                 KVALITETSAVVIKELSEKOD                   
004000*                                 0=INGEN ANM.  1=AVVIKELSE               
004100*                                 2=AVVIKELSE, RETURNERAS                 
004200*                                 QUALITY DEVIATION CODE                  
004300*                                 0 = NO DEV.  1 = DEVIATION              
004400*                                 2 = DEVIATION, WILL BE RETURNED         
004500     03 KVRETUR              PIC S9(7)           COMP-3.                  
004600*                                 ANTAL I RETUR                           
004700*                                 QUANTITY IN RETURN                      
004800     03 FILLER               PIC X(6).                                    
004900*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
