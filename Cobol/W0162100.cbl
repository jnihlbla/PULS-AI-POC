000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0162100.                                                
000300 AUTHOR.         PAF.                                                     
000400 DATE-WRITTEN.   20010124.                                                
000410 DATE-COMPILED.                                                           
000600                                                                          
000601*            KONVERTERING AV 1300.                                        
000620                                                                          
000630 DATA DIVISION.                                                           
000640                                                                          
000650 WORKING-STORAGE SECTION.                                                 
000651                                                                          
000652*    -- CHECKED BY WY2000                                                 
000660 01 W-COUNT          PIC S9(9) COMP-3  VALUE +0.                          
000670 77  IDPGM                     PIC X(8)    VALUE 'W0162100'.              
000680                                                                          
000690 LINKAGE SECTION.                                                         
000691 01  RECORD-F        PIC 9(8) COMP.                                       
000692     88 FIRST-REC    VALUE 00.                                            
000693     88 MIDDLE-REC   VALUE 04.                                            
000694     88 END-REC      VALUE 08.                                            
000696     EJECT                                                                
000697 01  INAREA.                                                              
000698     03 FILLER         PIC X(49).                                         
000707     03 I-IDFKN        PIC S9(5) COMP-3.                                  
000710     03 FILLER       PIC X(165).                                          
000711     EJECT                                                                
000712 01  UTAREA.                                                              
000713 02  UTAREA1.                                                             
000716     03 FILLER       PIC X(217).                                          
000725     02 IDFKNACK     PIC S9(1) COMP-3.                                    
000726     EJECT                                                                
000727 PROCEDURE DIVISION USING RECORD-F, INAREA, UTAREA.                       
000728                                                                          
000729     IF END-REC                                                           
000730       DISPLAY 'SLUT       '                                              
000731       MOVE 8            TO RETURN-CODE                                   
000732       GOBACK                                                             
000733     END-IF                                                               
000734     ADD +1 TO W-COUNT                                                    
000735     MOVE INAREA  TO UTAREA1                                              
000778     EVALUATE TRUE                                                        
000779     WHEN I-IDFKN < 1000 AND I-IDFKN NOT < 0 MOVE 0 TO IDFKNACK           
000780     WHEN I-IDFKN < 2000 AND I-IDFKN > 999 MOVE 1 TO IDFKNACK             
000781     WHEN I-IDFKN < 3000 AND I-IDFKN > 1999 MOVE 2 TO IDFKNACK            
000783     WHEN I-IDFKN < 4000 AND I-IDFKN > 2999 MOVE 3 TO IDFKNACK            
000784     WHEN I-IDFKN < 5000 AND I-IDFKN > 3999 MOVE 4 TO IDFKNACK            
000785     WHEN I-IDFKN < 6000 AND I-IDFKN > 4999 MOVE 5 TO IDFKNACK            
000786     WHEN I-IDFKN < 7000 AND I-IDFKN > 5999 MOVE 6 TO IDFKNACK            
000787     WHEN I-IDFKN < 8000 AND I-IDFKN > 6999 MOVE 7 TO IDFKNACK            
000788     WHEN I-IDFKN < 9000 AND I-IDFKN > 7999 MOVE 8 TO IDFKNACK            
000789     WHEN OTHER   MOVE 9 TO IDFKNACK                                      
000790     END-EVALUATE                                                         
000808     MOVE 20           TO RETURN-CODE                                     
000809                                                                          
000810     GOBACK                                                               
000900     .                                                                    
