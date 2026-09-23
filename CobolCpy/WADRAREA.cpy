000100 01  ADR-WADRAREA.                                                        
000200*                                 PARAMETERAREA TILL WADRBER.             
000300*                                 MODULER BOKAS ELLER AVBOKAS             
000400*                                 BEROENDE PÅ KDCALL.                     
000500*                                 ----- INDATA ------------------         
000600*                                 KDCALL  : 1 --> SÖK LEDIG PLATS         
000700*                                                 OCH BOKA OM             
000800*                                                 PLATS FINNS             
000900*                                           2 --> BOKA GIVEN AREA         
001000*                                           3 --> AVBOKA GIVEN            
001100*                                                 AREA                    
001200*                                           4 --> ANGE UPPTAGEN           
001300*                                                 PLATS                   
001400*                                 ADVMODUL: FYLLS I OM KDCALL             
001500*                                                 = 2, 3 ELLER 4          
001600*                                 ADHMODUL: FYLLS I OM KDCALL             
001700*                                                 = 2 ELLER 3             
001800*                                 KVMODUL : ANTAL MODULER SOM             
001900*                                           SKA BOKAS. FYLLS I OM         
002000*                                           KDCALL = 1                    
002100*                                 TBSPAERR: FRÅN DATABAS                  
002200*                                 ----- ANROP  ------------------         
002300*                                 CALL WADRBER USING ADR-WADRAREA         
002400*                                 ----- SVAR   ------------------         
002500*                                 ADVMODUL: VÄNSTERMODUL I AREAN          
002600*                                 ADHMODUL: HÖGERMODUL I AREAN            
002700*                                 KVMODUL : ORÖRD OM KDCALL EJ 4          
002800*                                 TBSPAERR: EFTER MODIFIERING             
002900*                                 KDSVAR  : BLANK --> UPPDRAG OK          
003000*                                           1 --> EJ NUM DATA             
003100*                                           2 --> ORIML VÄRDEN            
003200*                                           3 --> PLATS FANNS EJ          
003300*                                           4 --> AREAN SLUT              
003400     03 ADR-KDCALL           PIC S9(3)           COMP-3.                  
003500*                                 ANROPSTYP                               
003600     03 ADR-ADVMODUL         PIC S9(3)           COMP-3.                  
003700*                                 VÄNSTER-MODUL                           
003800     03 ADR-ADHMODUL         PIC S9(3)           COMP-3.                  
003900*                                 HÖGER-MODUL                             
004000     03 ADR-KVMODUL          PIC S9(3)           COMP-3.                  
004100*                                 ANTAL MODULER PER HYLLA                 
004200     03 ADR-TBSPAERR         PIC X(50).                                   
004300*                                 ANGER I BIT-MAPSFORM OM EN              
004400*                                 MODUL ÄR LEDIG ELLER UPPTAGEN.          
004500*                                 EN BIT PER MODUL I NIVÅN PÅ             
004600*                                 STÄLLAGE. VARJE NIVÅ HAR                
004700*                                 400 MODULER.                            
004800*                                 BITEN = 1 = UPPTAGEN                    
004900*                                 BITEN = 0 = LEDIG                       
005000     03 ADR-KDSVAR           PIC X.                                       
005100*                                 SVARSKOD FRÅN SUBPROGRAM                
005200*** END COPY WADRAREAC0  LENGTH=59                                        
