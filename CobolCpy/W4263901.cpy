000100 01  W4263901.                                                            
000200*                                 KVALITET KONTROLLRAPPORT                
000300*                                 JUSTERINGAR EKONOMI                     
000400*                                                                         
000500*                                 QUALITY CONTROL REPORT                  
000600*                                 ADJUSTMENTS, ECONOMY SYS.               
000700*                                                                         
000800     03 IDKR                 PIC 9(5).                                    
000900*                                 KONTROLLRAPPORT NUMMER                  
001000*                                 INSPECTION REPORT NUMBER                
001100     03 FLANNULL             PIC X.                                       
001200*                                 ANNULLATION                             
001300*                                 CANCELLATION                            
001400     03 IDARTNR              PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 IDFTG                PIC 9(2).                                    
001800*                                 F÷RETAGSID EKONOM REDOVISNING           
001900*                                 COMPANY IDENTITY ACCOUNTING             
002000     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
002100*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
002200*                                 (0VVDLLLLK)                             
002300*                                 SERIAL NO RECEIVING REPORT              
002400*                                 (0WWDLLLLC)                             
002500     03 IDDC                 PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 KDKRJUST             PIC X.                                       
002900*                                 JUSTERINGSKOD                           
003000*                                 ADJUSTMENT CODE                         
003100     03 KDKRSTA              PIC X.                                       
003200*                                 KONTROLLRAPPORT STATUS                  
003300*                                 INSPECTION REPORT STATUS                
003400     03 KVARBTID             PIC S9(2)V9(1)      COMP-3.                  
003500*                                 ANTAL MANTIMMAR                         
003600*                                 NUMBER OF MAN HOURS                     
003700     03 KVART-RET            PIC S9(7)           COMP-3.                  
003800*                                 ANTAL ARTIKLAR I RETUR                  
003900*                                 QUANTITY INSPECTED PARTS                
004000     03 KVART-SJUST          PIC S9(7)           COMP-3.                  
004100*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
004200*                                 QUANTITY INSPECTED PARTS                
004300     03 KVART-SKROT          PIC S9(7)           COMP-3.                  
004400*                                 ANTAL SKROTADE ARTIKLAR                 
004500*                                 QUANTITY INSPECTED PARTS                
004600     03 SUMAT                PIC S9(7)V9(2)      COMP-3.                  
004700*                                 MATERIALKOSTNAD                         
004800     03 SUOMK                PIC S9(7)           COMP-3.                  
004900*                                 BELOPP SOM SKALL DEBITERAS              
005000*                                 KUND                                    
005100*                                 AMOUNT TO BE PAID BY CUSTOMER           
005200     03 TIREGDAT             PIC S9(7)           COMP-3.                  
005300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005400*                                 REGISTRATION DATE (YYMMDD)              
005500*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
