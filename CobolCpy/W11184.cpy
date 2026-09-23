000100 01  W11184-CTX.                                                          
000200*                                 UPPGIFTER OM FARLIGT                    
000300*                                 GODS ARTIKLAR                           
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BEEMBMAT             PIC X(15).                                   
000700*                                 BENÄMNING EMBALLAGE MATERIAL            
000800     03 FLFROST              PIC X.                                       
000900*                                 FROSTKÄNSLIG                            
001000     03 FLTACTIL             PIC X.                                       
001100*                                 VARNINGSMÄRKE FÖR SYNSKADADE            
001200     03 FLVARINF             PIC X.                                       
001300*                                 VARU-INFO KEMISKA PRODUKTER             
001400     03 FLVARINF-SDS         PIC X.                                       
001500*                                 SAFETY DATA SHEET                       
001600     03 IDAO-FG              PIC X(10).                                   
001700*                                 ÄNDRINGSORDER NR FARLIGT GODS           
001800     03 IDVARINF             PIC X(4).                                    
001900*                                 ID VARUINFO KEMISKA PRODUKTER           
002000     03 IDVARINF-SDS         PIC X(4).                                    
002100*                                 ID SAFETY DATA SHEET                    
002200     03 KDFARG               PIC 9(3).                                    
002300*                                 FÄRG KOD FARLIGT GODS                   
002400     03 KDFGPRIO             PIC 9(3).                                    
002500*                                 HANTERINGSPRIO FARLIGT GODS             
002600     03 KDLACK               PIC X(2).                                    
002700*                                 LACKTYP                                 
002800     03 KDSORT-KVNTOFG       PIC X(2).                                    
002900*                                 SORT-KOD NETTOVIKT FARLIGT GODS         
003000     03 KDSORT-VLFG          PIC X(4).                                    
003100*                                 SORT-KOD VOLYM FARLIGT GODS             
003200     03 KVFLAMP              PIC S9(3)           COMP-3.                  
003300*                                 FLAMPUNKT FÖR FARLIGT GODS              
003400     03 KVNTOFG              PIC S9(2)V9(3)      COMP-3.                  
003500*                                 NETTOINNEHÅLL FARLIGT GODS              
003600     03 KVVOC                PIC S9(2)V9(3)      COMP-3.                  
003700*                                 HALT AV LÖSNINGSMEDEL PER KILO          
003800     03 SUEQFG               PIC S9(3)V9(4)      COMP-3.                  
003900*                                 EQ-VÄRDE FARLIGT GODS                   
004000     03 VKART-FG             PIC S9(7)           COMP-3.                  
004100*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
004200     03 VKFORSFG             PIC S9(4)V9(3)      COMP-3.                  
004300*                                 FÖRSÄLJNINGSVIKT FARLIGT GODS           
004400     03 VLFG                 PIC S9(4)V9(3)      COMP-3.                  
004500*                                 VOLYM FARLIGT GODS                      
004600     03 TENOTE               OCCURS 2 TIMES                               
004700                             PIC X(40).                                   
004800*                                 NOTERINGSFÄLT                           
004900     03 TIREGDAT             PIC S9(7)           COMP-3.                  
005000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005100     03 IDPSN                PIC 9(3).                                    
005200*                                 PROPER SHIPPING NAME                    
005300     03 KDFARLIG             PIC S9              COMP-3.                  
005400*                                 KOD FÖR FARLIGT GODS                    
005500     03 KDARTHNT-H           PIC S9(3)           COMP-3.                  
005600*                                 HANTERINGSKOD HÖGRA DELEN               
005700     03 KDERS                PIC S9(3)           COMP-3.                  
005800*                                 ERSÄTTNINGSKOD                          
005900     03 IDARTNR-RECEPT       PIC S9(9)           COMP-3.                  
006000*                                 ARTIKELNUMMER FÖR KEMI                  
006100     03 IDANMNR              PIC 9(6).                                    
006200*                                 A-NUMMER                                
006300     03 REKSIFFR-ANMNR       PIC S9              COMP-3.                  
006400*                                 KONTR.SIFFRA A-NUMMER                   
006500*** END OF VILMAII-COPY LENGTH= 184 BYTES                                 
