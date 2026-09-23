000100 01  MID-W90412I1-CTX.                                                    
000200*                                 MID-COPYTEXT FÖR W9041200               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-FILLERN3         PIC 9(3).                                    
000800     03 MID-INPUT.                                                        
000900*                                 INRAPPORTERINGSDEL 9412                 
001000        05 MID-KDFARG        PIC 9(3).                                    
001100*                                 FÄRG KOD FARLIGT GODS                   
001200        05 MID-KDLACK        PIC X(2).                                    
001300*                                 LACKTYP                                 
001400        05 MID-IDARTNR-RECEPT                                             
001500                             PIC X(9).                                    
001600*                                 ARTIKELNUMMER FÖR KEMI                  
001700        05 MID-VKFORSFG      PIC X(8).                                    
001800*                                 FÖRSÄLJNINGSVIKT FARLIGT GODS           
001900        05 MID-FLVARINF      PIC X.                                       
002000*                                 VARU-INFO KEMISKA PRODUKTER             
002100        05 MID-IDVARINF      PIC X(4).                                    
002200*                                 ID VARUINFO KEMISKA PRODUKTER           
002300        05 MID-KVNTOFG       PIC X(6).                                    
002400*                                 NETTOINNEHÅLL FARLIGT GODS              
002500        05 MID-KDSORT-KVNTOFG                                             
002600                             PIC X(2).                                    
002700*                                 SORT-KOD                                
002800        05 MID-KVVOC         PIC X(6).                                    
002900*                                 HALT AV LÖSNINGSMEDEL PER KILO          
003000        05 MID-SUEQFG        PIC X(8).                                    
003100*                                 EQ-VÄRDE FARLIGT GODS                   
003200        05 MID-FLVARINF-SDS  PIC X.                                       
003300*                                 SAFETY DATA SHEET                       
003400        05 MID-IDVARINF-SDS  PIC X(4).                                    
003500*                                 ID SAFETY DATA SHEET                    
003600        05 MID-VLFG          PIC X(8).                                    
003700*                                 VOLYM FARLIGT GODS                      
003800        05 MID-KDSORT-VLFG   PIC X(4).                                    
003900*                                 SORT-KOD VOLYM FARLIGT GODS             
004000        05 MID-FLNEG         PIC X.                                       
004100*                                 ALLMÄN FLAGGA                           
004200        05 MID-KVFLAMP       PIC 9(3).                                    
004300*                                 FLAMPUNKT FÖR FARLIGT GODS              
004400        05 MID-FLFROST       PIC X.                                       
004500*                                 FROSTKÄNSLIG                            
004600        05 MID-IDPSN-IN      PIC 9(3).                                    
004700*                                 PROPER SHIPPING NAME                    
004800        05 MID-IDAO-FG       PIC X(10).                                   
004900*                                 ÄNDRINGSORDER NR FARLIGT GODS           
005000        05 MID-KDFGPRIO      PIC 9(3).                                    
005100*                                 HANTERINGSPRIO FARLIGT GODS             
005200        05 MID-FLTACTIL      PIC X.                                       
005300*                                 VARNINGSMÄRKE FÖR SYNSKADADE            
005400        05 MID-BEEMBMAT      PIC X(15).                                   
005500*                                 BENÄMNING EMBALLAGE MATERIAL            
005600        05 MID-IDANMNR       PIC 9(6).                                    
005700*                                 A-NUMMER                                
005800        05 MID-REKSIFFR-ANMNR                                             
005900                             PIC 9.                                       
006000*                                 KONTR.SIFFRA A-NUMMER                   
006100        05 MID-VKART-FG      PIC 9(7).                                    
006200*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
006300        05 MID-TENOTE        OCCURS 2 TIMES                               
006400                             PIC X(40).                                   
006500*                                 NOTERINGSFÄLT                           
006600     03 MID-FLBORT           PIC X.                                       
006700*                                 ALLMÄN SVARSFLAGGA                      
006800*** END OF VILMAII-COPY LENGTH= 219 BYTES                                 
