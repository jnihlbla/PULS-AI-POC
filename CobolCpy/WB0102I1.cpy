000100 01  REQU-WB0102I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WB0102             
000300*                                 ACCESSORIES UPDATE                      
000400     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 REQU-IDUPPDSU-KEY    PIC 9(8).                                    
000800*                                 SU-UPPDRAGSNUMMER TIKO                  
000900*                                 SU COMMISSION IDENTITY NO               
001000     03 REQU-IDUPPDSU-D-KEY  PIC 9(8).                                    
001100*                                 SU-UPPDRAGSNUMMER TIKO                  
001200*                                 SU COMMISSION IDENTITY NO               
001300     03 REQU-IDUPPDSU-A-KEY  PIC 9(8).                                    
001400*                                 SU-UPPDRAGSNUMMER TIKO                  
001500*                                 SU COMMISSION IDENTITY NO               
001600     03 REQU-IDPRODGR        PIC X(4).                                    
001700*                                 PRODUKTGRUPP CHEF                       
001800*                                 PRODUCT GROUP MANAGER                   
001900     03 REQU-BEASSTYP        PIC X(7).                                    
002000*                                 TILLBEHÖR TILLDELNINGSTYP               
002100*                                 ACCESSORY ASSIGNMENT TYPE               
002200     03 REQU-KDFRPTYP        PIC X.                                       
002300*                                 TYP AV FÖRPACKNING                      
002400*                                 TYPE OF PACKAGE                         
002500     03 REQU-KVYVOL-INT      PIC 9(6).                                    
002600*                                 BESL. ÅRSVOL FÖR INTROD.                
002700*                                 DECIDED YR VOL TO LAUNCH                
002800     03 REQU-KVYVOL-B3       PIC 9(6).                                    
002900*                                 JUST.AV ÅRSVOLYM BOARD 3                
003000*                                 ADJ.OF YEAR VOLUME BOARD 3              
003100     03 REQU-KVYVOL-B2       PIC 9(6).                                    
003200*                                 JUST.AV ÅRSVOLYM BOARD 2                
003300*                                 ADJ. OF YEAR VOLUME BOARD 2             
003400     03 REQU-KVYVOL-B1       PIC 9(6).                                    
003500*                                 JUSTERAD ÅRSVOLYM BOARD 1               
003600*                                 ADJUSTED YEAR VOLUME BOARD 1            
003700     03 REQU-KVYVOL-ASS      PIC 9(6).                                    
003800*                                 ÅSATT ÅRSVOLYM I LAGER                  
003900*                                 ASSIGNED YEAR VOLUME                    
004000     03 REQU-BEMAPP          PIC X(5).                                    
004100*                                 MAPP                                    
004200*                                 BINDER                                  
004300     03 REQU-KVFOTO          PIC 9(2).                                    
004400*                                 ANTAL FOTOGRAFIER                       
004500*                                 NUMBER OF PHOTOS                        
004600     03 REQU-TIFOTO          PIC 9(4).                                    
004700*                                 FOTO MTRL VECKA                         
004800*                                 PHOTO MTRL WEEK                         
004900     03 REQU-TEVERKTYG       PIC X(4).                                    
005000*                                 VERKTYGSKÖP INFO                        
005100     03 REQU-TESTATXT        PIC X(25).                                   
005200*                                 STA INFORMATION                         
005300     03 REQU-TEMATXT         PIC X(25).                                   
005400*                                 MA INFORMATION                          
005500     03 REQU-TEINKTXT        PIC X(25).                                   
005600*                                 INK INFORMATION                         
005700     03 REQU-TEANSTXT        PIC X(25).                                   
005800*                                 ANSK INFORMATION                        
005900     03 REQU-TEAUXTXT        PIC X(25).                                   
006000*                                 EXTRA NOTERING                          
006100*** END OF VILMAII-COPY LENGTH= 214 BYTES                                 
