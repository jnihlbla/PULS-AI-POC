000010*** EDIT ALLOWED                                                          
000100*       +---------------------------------------------+                   
000200*       ! KVALITETSUPPFÖLJNING FÖR DC=11 (GÖTEBORG)   !                   
000300*       !               - LAGEROMRÅDE                 !                   
000400*       !               - BENÄMNING LAGEROMRÅDE, (S)  !                   
000410*       !               - BENÄMNING LAGEROMRÅDE, (GB) !                   
000500*       !               - OMRÄKNINGSTAL               !                   
000600*       !               - ANTAL URVALSGRUPPER         !                   
000601*       !                                             !                   
000602*       ! INCLUDE ÄVEN PÅ WWKVOM21 - 26  FÖR SDC:ERNA !                   
000610*       +---------------------------------------------+                   
000620                                                                          
000800 01  KVA-LAGERINDELNING.                                                  
000900    03 CDC-LAGERINDELNING.                                                
000910       05 CDC-ENTRESOLEN.                                                 
001000          07 FILLER        PIC X(01)        VALUE 'A'.                    
001100          07 FILLER        PIC X(10)        VALUE 'ENTRESOLEN'.           
001300          07 FILLER        PIC X(10)        VALUE 'MEZZANINE '.           
001400          07 FILLER        PIC 9(02)V9(01)  VALUE 6.8.                    
001600          07 FILLER        PIC 9(01)        VALUE 4.                      
001800       05 CDC-PALL.                                                       
001900          07 FILLER        PIC X(01)        VALUE 'B'.                    
002000          07 FILLER        PIC X(10)        VALUE 'PALL      '.           
002200          07 FILLER        PIC X(10)        VALUE 'PALLET    '.           
002300          07 FILLER        PIC 9(02)V9(01)  VALUE 3.0.                    
002500          07 FILLER        PIC 9(01)        VALUE 3.                      
002700       05 CDC-OVRIG-PALL.                                                 
002800          07 FILLER        PIC X(01)        VALUE 'C'.                    
002900          07 FILLER        PIC X(10)        VALUE 'ÖVRIG-PALL'.           
003100          07 FILLER        PIC X(10)        VALUE 'REMAINING '.           
003300          07 FILLER        PIC 9(02)V9(01)  VALUE 9.0.                    
003500          07 FILLER        PIC 9(01)        VALUE 1.                      
003510       05 CDC-GROV.                                                       
003520          07 FILLER        PIC X(01)        VALUE 'D'.                    
003530          07 FILLER        PIC X(10)        VALUE 'GROV      '.           
003550          07 FILLER        PIC X(10)        VALUE 'LARGE     '.           
003570          07 FILLER        PIC 9(02)V9(01)  VALUE 3.7.                    
003590          07 FILLER        PIC 9(01)        VALUE 3.                      
003591       05 CDC-72.                                                         
003592          07 FILLER        PIC X(01)        VALUE 'E'.                    
003595          07 FILLER        PIC X(10)        VALUE '72        '.           
003596          07 FILLER        PIC X(10)        VALUE '72        '.           
003597          07 FILLER        PIC 9(02)V9(01)  VALUE 3.0.                    
003599          07 FILLER        PIC 9(01)        VALUE 2.                      
003601       05 CDC-SPECIAL.                                                    
003602          07 FILLER        PIC X(01)        VALUE 'F'.                    
003603          07 FILLER        PIC X(10)        VALUE 'SPECIAL   '.           
003604          07 FILLER        PIC X(10)        VALUE 'SPECIAL   '.           
003607          07 FILLER        PIC 9(02)V9(01)  VALUE 6.8.                    
003609          07 FILLER        PIC 9(01)        VALUE 2.                      
003610*                                                                         
003612*    03 DC21-LAGERINDELNING  -COPY WWKVOM21                               
003614*    03 DC22-LAGERINDELNING  -COPY WWKVOM22                               
003616*    03 DC23-LAGERINDELNING  -COPY WWKVOM23                               
003618*    03 DC24-LAGERINDELNING  -COPY WWKVOM24                               
003620*    03 DC25-LAGERINDELNING  -COPY WWKVOM25                               
003621*    03 DC26-LAGERINDELNING  -COPY WWKVOM26                               
003623*                                                                         
003624 01  FILLER REDEFINES KVA-LAGERINDELNING.                                 
003625     03 FILLER    OCCURS 7 TIMES.                                         
003630*                                      CDC + SEX SDC                      
003700        05 FILLER   OCCURS 6 TIMES.                                       
003710*                                      OMRÅDEN INOM ETT DC                
003800          07 KVA-IDKVAOMR         PIC X(01).                              
003900          07 KVA-BEKVAOMR-DC      PIC X(10).                              
004100          07 KVA-BEKVAOMR-GB      PIC X(10).                              
004200          07 KVA-REKVAOMR         PIC 9(02)V9(01).                        
004400          07 KVA-KVKVAURV         PIC 9(01).                              
