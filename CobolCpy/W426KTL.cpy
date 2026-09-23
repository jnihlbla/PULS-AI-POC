000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** KVALITET                                 
000300*                            ***  - VERIFIKATION/NORMAL KONTROLL          
000310*                            ***  - SPECIAL KONTROLL/KONT.RAPPORT         
000400*                            ***  - UNDERLAG FÖR KVALITETSKONTROLL        
000700*                            *************************************        
000710 01  KVA-KDKVATYP1.                                                       
000900     03 KVA-KDKVATYP-S.                                                   
001120       05  FILLER   PIC X(15) VALUE 'NORMAL KONTROLL'.                    
001121       05  FILLER   PIC X(15) VALUE 'VER.KONTROLL   '.                    
001122     03 KVA-KDKVATYP-GB.                                                  
001123       05  FILLER   PIC X(15) VALUE 'NORMAL INSP.   '.                    
001124       05  FILLER   PIC X(15) VALUE 'VERIFICATION   '.                    
001125     03 KVA-KDKVATYP-B.                                                   
001126       05  FILLER   PIC X(15) VALUE 'NORMALE INSP.  '.                    
001127       05  FILLER   PIC X(15) VALUE 'IDENTIFICATIE  '.                    
001128 01  FILLER REDEFINES KVA-KDKVATYP1.                                      
001129     03 FILLER  OCCURS 3 TIMES.                                           
001130       05 KVA-KDKVATYP OCCURS 2 TIMES PIC X(15).                          
001131                                                                          
001132 01  KVA-KONTROLLTYP1.                                                    
001133     03 KVA-KONTROLLTYP-S.                                                
001134       05  FILLER   PIC X(17) VALUE 'KONTROLLRAPPORT  '.                  
001135       05  FILLER   PIC X(17) VALUE 'SPECIALKONTROLL  '.                  
001136     03 KVA-KONTROLLTYP-GB.                                               
001137       05  FILLER   PIC X(17) VALUE 'INSPECTION REPORT'.                  
001138       05  FILLER   PIC X(17) VALUE 'SPECIAL INSP.    '.                  
001139     03 KVA-KONTROLLTYP-B.                                                
001140       05  FILLER   PIC X(17) VALUE 'INSPEKTIE REPORT '.                  
001141       05  FILLER   PIC X(17) VALUE 'SPECIAL INSP.    '.                  
001142 01  FILLER REDEFINES KVA-KONTROLLTYP1.                                   
001143     03 FILLER  OCCURS 3 TIMES.                                           
001144       05 KVA-KONTROLLTYP OCCURS 2 TIMES PIC X(17).                       
001145                                                                          
001146 01  KVA-KVALITETSUNDERLAG.                                               
001147     03 KVA-BEKVAULG-S.                                                   
001150       05  FILLER   PIC X(15) VALUE 'KI EGEN        '.                    
001200       05  FILLER   PIC X(15) VALUE 'KI LOTUS       '.                    
001300       05  FILLER   PIC X(15) VALUE 'RITNING        '.                    
001400       05  FILLER   PIC X(15) VALUE 'TILL FOTO      '.                    
001410       05  FILLER   PIC X(15) VALUE 'KI FOTO        '.                    
001411       05  FILLER   PIC X(15) VALUE 'LIKARE         '.                    
001412       05  FILLER   PIC X(15) VALUE 'CROSSINDEX     '.                    
001413       05  FILLER   PIC X(15) VALUE 'TILL LIKARE    '.                    
001414       05  FILLER   PIC X(15) VALUE '               '.                    
001415       05  FILLER   PIC X(15) VALUE '               '.                    
001423     03 KVA-BEKVAULG-GB.                                                  
001430       05  FILLER   PIC X(15) VALUE 'PARTS INSTR.   '.                    
001440       05  FILLER   PIC X(15) VALUE 'LOTUS INSTR.   '.                    
001450       05  FILLER   PIC X(15) VALUE 'DRAWING        '.                    
001460       05  FILLER   PIC X(15) VALUE 'PHOTO PLANNED  '.                    
001470       05  FILLER   PIC X(15) VALUE 'PHOTO INSTR.   '.                    
001480       05  FILLER   PIC X(15) VALUE 'SAMPLE INSTR.  '.                    
001490       05  FILLER   PIC X(15) VALUE 'CROSS INDEX    '.                    
001491       05  FILLER   PIC X(15) VALUE 'SAMP INSTR PLAN'.                    
001492       05  FILLER   PIC X(15) VALUE '               '.                    
001493       05  FILLER   PIC X(15) VALUE '               '.                    
001500     03 KVA-BEKVAULG-B.                                                   
001700       05  FILLER   PIC X(15) VALUE 'PARTS INSTR.   '.                    
001800       05  FILLER   PIC X(15) VALUE 'LOTUS INSTR.   '.                    
001900       05  FILLER   PIC X(15) VALUE 'TEKENING       '.                    
002000       05  FILLER   PIC X(15) VALUE 'FOTO GEPLAND   '.                    
002100       05  FILLER   PIC X(15) VALUE 'FOTO INSTR.    '.                    
002200       05  FILLER   PIC X(15) VALUE 'SAMPLE INSTR.  '.                    
002300       05  FILLER   PIC X(15) VALUE 'X-INDEX        '.                    
002400       05  FILLER   PIC X(15) VALUE 'SAMP INSTR GEPL'.                    
002410       05  FILLER   PIC X(15) VALUE '               '.                    
002420       05  FILLER   PIC X(15) VALUE '               '.                    
003600 01  FILLER REDEFINES KVA-KVALITETSUNDERLAG.                              
003700     03 FILLER  OCCURS 3 TIMES.                                           
003710       05 KVA-BEKVAULG OCCURS 10 TIMES PIC X(15).                         
