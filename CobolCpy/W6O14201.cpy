000100 01  MOD-W6O14201.                                                        
000200*                                 COPYTEXT FOR MOD W6O14201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDILIST-IN       PIC X(5).                                    
000800*                                 INLÄGGNINGSLISTEIDENTITET               
000900     03 MOD-IDILIST-UT       PIC X(5).                                    
001000*                                 INLÄGGNINGSLISTEIDENTITET               
001100     03 MOD-IDILIRAD-IN      PIC X(5).                                    
001200*                                 INLÄGGNINGSLISTERADNUMMER               
001300     03 MOD-IDILIRAD-UT      PIC X(5).                                    
001400*                                 INLÄGGNINGSLISTERADNUMMER               
001500     03 MOD-IDILIRAD-ENTER   PIC 9(5).                                    
001600*                                 INLÄGGNINGSLISTERADNUMMER               
001700     03 MOD-IDILIRAD-NEXT    PIC 9(5).                                    
001800*                                 INLÄGGNINGSLISTERADNUMMER               
001900     03 MOD-FLKLAR-ATTR      PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-FLKLAR           PIC X.                                       
002200*                                 AVSLUTNINGSMARKERING                    
002300     03 MOD-IDANSTNR-ATTR    PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-IDANSTNR         PIC Z(4)9.                                   
002600*                                 ANSTÄLLNINGSNUMMER                      
002700     03 MOD-FLKLAR-MAK-ATTR  PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLKLAR-MAK       PIC X.                                       
003000*                                 AVSLUTNINGSMARKERING                    
003100     03 MOD-FILLER           OCCURS 12 TIMES.                             
003200*                                 UPDATE                                  
003300        05 MOD-KDCMDVAL-RAD-ATTR                                          
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-KDCMDVAL-RAD  PIC X(3).                                    
003700*                                 GENERELL KOMMANDOKOD                    
003800     03 MOD-FILLER           OCCURS 12 TIMES.                             
003900*                                 UPDATE                                  
004000        05 MOD-KVINLART-UPD-ATTR                                          
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-KVINLART-UPD  PIC X(6).                                    
004400*                                 ANTAL I PARTIRAD                        
004500     03 MOD-FILLER           OCCURS 12 TIMES.                             
004600*                                 UPDATE                                  
004700        05 MOD-ADINLOMR-NXT-UPD-ATTR                                      
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-ADINLOMR-NXT-UPD                                           
005100                             PIC X(4).                                    
005200*                                 INLEVERANSOMRÅDE                        
005300     03 MOD-IDILIRAD-RAD     OCCURS 12 TIMES                              
005400                             PIC Z(4)9.                                   
005500*                                 INLÄGGNINGSLISTERADNUMMER               
005600     03 MOD-ADLAGOMR-RAD     OCCURS 12 TIMES                              
005700                             PIC Z9.                                      
005800*                                 LAGEROMRÅDE                             
005900     03 MOD-ADGANG-RAD       OCCURS 12 TIMES                              
006000                             PIC Z9.                                      
006100*                                 GÅNG                                    
006200     03 MOD-ADPLATS-RAD      OCCURS 12 TIMES                              
006300                             PIC Z(4)9.                                   
006400*                                 LAGERPLATSNUMMER                        
006500     03 MOD-FILLER           OCCURS 12 TIMES.                             
006600*                                 RAD                                     
006700        05 MOD-IDARTNR-RAD-ATTR                                           
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-IDARTNR-RAD   PIC Z(7)9.                                   
007100*                                 ARTIKELNUMMER                           
007200     03 MOD-BEART-RAD        OCCURS 12 TIMES                              
007300                             PIC X(23).                                   
007400     03 MOD-KVINLART-RAD     OCCURS 12 TIMES                              
007500                             PIC Z(5)9.                                   
007600*                                 ANTAL I PARTIRAD                        
007700     03 MOD-ADINLOMR-FB-RAD  OCCURS 12 TIMES                              
007800                             PIC X(4).                                    
007900*                                 INLEVERANSOMRÅDE                        
008000     03 MOD-TEMFSINF         PIC X(55).                                   
008100*                                 INFORMATIONSMEDDELANDE                  
008200*** END COPY W6O14201    LENGTH=1054                                      
