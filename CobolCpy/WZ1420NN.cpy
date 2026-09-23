000100***   EDIT ALLOWED                                                        
000200 01     AREA.                                                             
000300   03   RETCODE          PIC S9(9) BINARY VALUE ZEROES.                   
000400   03   DATA             PIC X(050)      VALUE SPACES.                    
000500   03   FILLER  REDEFINES DATA.                                           
000600     05 DATA-CH OCCURS 50 TIMES                                           
000700            INDEXED BY INX1 INX2 INX3                                     
000800            PIC X(001).                                                   
000900   03   EXITPGM          PIC X(008)       VALUE SPACES.                   
001000   03   FOLDER           PIC X(040)       VALUE SPACES.                   
001100   03   LIST-ENTRY.                                                       
001200     05 REFID            PIC X(016).                                      
001300     05 SUBJECT          PIC X(256).                                      
001400     05 MAIL-ADDR        PIC X(256).                                      
001500     05 DATE             PIC X(032).                                      
001600     05 STATUS           PIC X(032).                                      
001700     05 CONTENT-TYPE     PIC X(032).                                      
001800     05 AGE              PIC S9(004)      VALUE ZEROES.                   
001900     05 FILLER           PIC X(002).                                      
