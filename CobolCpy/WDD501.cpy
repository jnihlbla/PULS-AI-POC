000100 01  ART-WDD501.                                                          
000200*                                 ARTIKEL REGISTER                        
000300*                                 FARLIGT GODS INFORMATION                
000400*                                 FYSISK NYCKEL: IDARTNR                  
000500     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 ART-BEEMBMAT         PIC X(15).                                   
000900*                                 BENÄMNING EMBALLAGE MATERIAL            
001000*                                 DESCRIPTION OF PACKING MATERIAL         
001100     03 ART-FLFROST          PIC X.                                       
001200*                                 FROSTKÄNSLIG                            
001300*                                 SENSITIVE FOR FROST                     
001400     03 ART-FLTACTIL         PIC X.                                       
001500*                                 VARNINGSMÄRKE FÖR SYNSKADADE            
001600*                                 WARNING SIGN DEFECTIVE VISION           
001700     03 ART-FLVARINF         PIC X.                                       
001800*                                 VARU-INFO KEMISKA PRODUKTER             
001900*                                 INFORMATION CHEMICAL PRODUCTS           
002000     03 ART-FLVARINF-SDS     PIC X.                                       
002100*                                 SAFETY DATA SHEET                       
002200*                                 SAFETY DATA SHEET                       
002300     03 ART-IDAO-FG          PIC X(10).                                   
002400*                                 ÄNDRINGSORDER NR FARLIGT GODS           
002500*                                 DCN NUMBER DANGEROUS GODS               
002600     03 ART-IDVARINF         PIC X(4).                                    
002700*                                 ID VARUINFO KEMISKA PRODUKTER           
002800*                                 ID INFO. CHEMICAL PRODUCTS              
002900     03 ART-IDVARINF-SDS     PIC X(4).                                    
003000*                                 ID SAFETY DATA SHEET                    
003100*                                 ID SAFETY DATA SHEET                    
003200     03 ART-KDFARG           PIC 9(3).                                    
003300*                                 FÄRG KOD FARLIGT GODS                   
003400*                                 COLOUR CODE DANGEROUS GOODS             
003500     03 ART-KDFGPRIO         PIC 9(3).                                    
003600*                                 HANTERINGSPRIO FARLIGT GODS             
003700*                                 PRIORITY CODE DANGEROUS GODS            
003800     03 ART-KDLACK           PIC X(2).                                    
003900*                                 LACKTYP                                 
004000*                                 TYPE OF PAINT                           
004100     03 ART-KDSORT-KVNTOFG   PIC X(2).                                    
004200*                                 SORT-KOD NETTOVIKT FARLIGT GODS         
004300*                                 SORT NET CONTENT DANGEROUS GODS         
004400     03 ART-KDSORT-VLFG      PIC X(4).                                    
004500*                                 SORT-KOD VOLYM FARLIGT GODS             
004600*                                 SORT-CODE VOLUME DANGEROUS GODS         
004700     03 ART-KVFLAMP          PIC S9(3)           COMP-3.                  
004800*                                 FLAMPUNKT FÖR FARLIGT GODS              
004900*                                 FLASH POINT FOR DANGEROUS GOODS         
005000     03 ART-KVNTOFG          PIC S9(2)V9(3)      COMP-3.                  
005100*                                 NETTOINNEHÅLL FARLIGT GODS              
005200*                                 NET CONTENT DANGEROUS GOODS             
005300     03 ART-KVVOC            PIC S9(2)V9(3)      COMP-3.                  
005400*                                 HALT AV LÖSNINGSMEDEL PER KILO          
005500*                                 SOLVENT PER KILO                        
005600     03 ART-SUEQFG           PIC S9(3)V9(4)      COMP-3.                  
005700*                                 EQ-VÄRDE FARLIGT GODS                   
005800*                                 EQ VALUE DANGEROUS GODS                 
005900     03 ART-VKART-FG         PIC S9(7)           COMP-3.                  
006000*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
006100*                                 NET WEIGHT EXPLOSIVES                   
006200     03 ART-VKFORSFG         PIC S9(4)V9(3)      COMP-3.                  
006300*                                 FÖRSÄLJNINGSVIKT FARLIGT GODS           
006400*                                 WEIGHT DANGEROUS GOODS                  
006500     03 ART-VLFG             PIC S9(4)V9(3)      COMP-3.                  
006600*                                 VOLYM FARLIGT GODS                      
006700*                                 VOLUME DANGEROUS GOODS                  
006800     03 ART-TENOTE           OCCURS 2 TIMES                               
006900                             PIC X(40).                                   
007000*                                 NOTERINGSFÄLT                           
007100*                                 NOTE FIELD                              
007200     03 ART-TIREGDAT         PIC S9(7)           COMP-3.                  
007300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007400*                                 REGISTRATION DATE (YYMMDD)              
007500     03 ART-IDARTNR-RECEPT   PIC S9(9)           COMP-3.                  
007600*                                 ARTIKELNUMMER FÖR KEMI                  
007700*                                 PART NUMBER KEMI                        
007800     03 ART-IDANMNR          PIC 9(6).                                    
007900*                                 A-NUMMER                                
008000*                                 A NUMBER                                
008100     03 ART-REKSIFFR-ANMNR   PIC S9              COMP-3.                  
008200*                                 KONTR.SIFFRA A-NUMMER                   
008300*                                 CHECK DIGIT FOR REPORT NO.              
008400     03 FILLER               PIC X(12).                                   
008500*** END OF VILMAII-COPY LENGTH= 188 BYTES                                 
