000100 01  MID-W1I11801-CTX.                                                    
000200*                                 MID-COPYTEXT FÖR W1011800               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDPSN-DOLT       PIC 9(3).                                    
000800*                                 PROPER SHIPPING NAME                    
000900     03 MID-INPUT.                                                        
001000*                                 INRAPPORTERINGSDEL 1118                 
001100        05 MID-KDFARG        PIC 9(3).                                    
001200*                                 FÄRG KOD FARLIGT GODS                   
001300        05 MID-KDLACK        PIC X(2).                                    
001400*                                 LACKTYP                                 
001500        05 MID-IDARTNR-RECEPT                                             
001600                             PIC X(9).                                    
001700*                                 ARTIKELNUMMER FÖR KEMI                  
001800        05 MID-VKFORSFG      PIC X(8).                                    
001900*                                 FÖRSÄLJNINGSVIKT FARLIGT GODS           
002000        05 MID-FLVARINF      PIC X.                                       
002100*                                 VARU-INFO KEMISKA PRODUKTER             
002200        05 MID-IDVARINF      PIC X(4).                                    
002300*                                 ID VARUINFO KEMISKA PRODUKTER           
002400        05 MID-KVNTOFG       PIC X(6).                                    
002500*                                 NETTOINNEHÅLL FARLIGT GODS              
002600        05 MID-KDSORT-KVNTOFG                                             
002700                             PIC X(2).                                    
002800*                                 SORT-KOD                                
002900        05 MID-KVVOC         PIC X(6).                                    
003000*                                 HALT AV LÖSNINGSMEDEL PER KILO          
003100        05 MID-SUEQFG        PIC X(8).                                    
003200*                                 EQ-VÄRDE FARLIGT GODS                   
003300        05 MID-FLVARINF-SDS  PIC X.                                       
003400*                                 SAFETY DATA SHEET                       
003500        05 MID-IDVARINF-SDS  PIC X(4).                                    
003600*                                 ID SAFETY DATA SHEET                    
003700        05 MID-VLFG          PIC X(8).                                    
003800*                                 VOLYM FARLIGT GODS                      
003900        05 MID-KDSORT-VLFG   PIC X(4).                                    
004000*                                 SORT-KOD VOLYM FARLIGT GODS             
004100        05 MID-FLNEG         PIC X.                                       
004200*                                 ALLMÄN FLAGGA                           
004300        05 MID-KVFLAMP       PIC 9(3).                                    
004400*                                 FLAMPUNKT FÖR FARLIGT GODS              
004500        05 MID-FLFROST       PIC X.                                       
004600*                                 FROSTKÄNSLIG                            
004700        05 MID-IDPSN         PIC 9(3).                                    
004800*                                 PROPER SHIPPING NAME                    
004900        05 MID-IDAO-FG       PIC X(10).                                   
005000*                                 ÄNDRINGSORDER NR FARLIGT GODS           
005100        05 MID-KDFGPRIO      PIC 9(3).                                    
005200*                                 HANTERINGSPRIO FARLIGT GODS             
005300        05 MID-FLTACTIL      PIC X.                                       
005400*                                 VARNINGSMÄRKE FÖR SYNSKADADE            
005500        05 MID-BEEMBMAT      PIC X(15).                                   
005600*                                 BENÄMNING EMBALLAGE MATERIAL            
005700        05 MID-IDANMNR       PIC 9(6).                                    
005800*                                 A-NUMMER                                
005900        05 MID-REKSIFFR-ANMNR                                             
006000                             PIC 9.                                       
006100*                                 KONTR.SIFFRA A-NUMMER                   
006200        05 MID-VKART-FG      PIC 9(7).                                    
006300*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
006400        05 MID-TENOTE        OCCURS 2 TIMES                               
006500                             PIC X(40).                                   
006600*                                 NOTERINGSFÄLT                           
006700     03 MID-FLBORT           PIC X.                                       
006800*                                 ALLMÄN SVARSFLAGGA                      
006900*** END OF VILMAII-COPY LENGTH= 219 BYTES                                 
