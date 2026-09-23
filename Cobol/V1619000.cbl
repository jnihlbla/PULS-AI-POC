000010**************************************************************            
000020 ID  DIVISION.                                                            
000021**************************************************************            
000040 PROGRAM-ID.    V1619000.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  OKTOBER 1990                                              
000111***************************************************************           
000112*                                                                         
000113*REMARKS:                                                                 
000114*                                                                         
000115*  * FÖRVANDLAR FRÅN YYMMDD (VIOS-FORMAT) TILL YYY-MM-DD (DB2-FORM        
000125*                                                                         
000126*  * PARAMETRAR:                                                          
000127*                                                                         
000128*        VIOS-DATUM                                                       
000129*        DB2-DATUM                                                        
000130*                                                                         
000133*  * RETURKODER:                                                          
000134*                                                                         
000137*                                                                         
000138***************************************************************           
000139                                                                          
000140**************************************************************            
000141 ENVIRONMENT DIVISION.                                                    
000142**************************************************************            
000160 CONFIGURATION SECTION.                                                   
000170 SOURCE-COMPUTER. IBM-370.                                                
000180*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
000190                                                                          
000191**************************************************************            
000210 DATA DIVISION.                                                           
000211**************************************************************            
000230 WORKING-STORAGE SECTION.                                                 
000240 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1619000'.             
000250 01  ABENDANDE-SECTION            PIC X(25) VALUE SPACE.                  
000260 01  RETUR-KODER.                                                         
000270     03  RKOD                     PIC S9(4) COMP SYNC VALUE ZERO.         
000280     03  RKOD-4                   PIC S9(4) COMP SYNC VALUE 4.            
000290     03  RKOD-8                   PIC S9(4) COMP SYNC VALUE 8.            
000300     03  RKOD-16                  PIC S9(4) COMP SYNC VALUE 16.           
000310     03  RKOD-20                  PIC S9(4) COMP SYNC VALUE 20.           
000320     EJECT                                                                
000330*-------------------------------- PARM-AREOR                              
000340 LINKAGE SECTION.                                                         
000350 01  PARM.                                                                
000360     03  VIOS-DATUM           PIC X(6).                                   
000370     03  DB2-DATUM            PIC X(10).                                  
000380     EJECT                                                                
000390*************************************************************             
000400 PROCEDURE DIVISION USING PARM.                                           
000410*************************************************************             
000420     PERFORM A-INIT                                                       
000430     PERFORM B-KONV-TILL-DB2-FORMAT                                       
000440     GOBACK                                                               
000450     CONTINUE.                                                            
000460                                                                          
000461*------------------------------------------------------------             
000470 A-INIT SECTION.                                                          
000471*------------------------------------------------------------             
000480     MOVE 'A-INIT                  ' TO ABENDANDE-SECTION                 
000490D    DISPLAY ABENDANDE-SECTION                                            
000500                                                                          
000510D    DISPLAY 'PARAMETRAR '                                                
000520D    DISPLAY DB2-DATUM                                                    
000530D    DISPLAY VIOS-DATUM                                                   
000540     CONTINUE.                                                            
000550                                                                          
000551*------------------------------------------------------------             
000560 B-KONV-TILL-DB2-FORMAT SECTION.                                          
000561*------------------------------------------------------------             
000570     MOVE 'B-KONV-TILL-DB2-FORMAT  ' TO ABENDANDE-SECTION                 
000580D    DISPLAY ABENDANDE-SECTION                                            
000590                                                                          
000600     IF VIOS-DATUM = SPACE THEN                                           
000610        ACCEPT VIOS-DATUM FROM DATE                                       
000620     END-IF                                                               
000630     MOVE '19'            TO DB2-DATUM(1:2)                               
000640     MOVE VIOS-DATUM(1:2) TO DB2-DATUM(3:2)                               
000650     MOVE '-'             TO DB2-DATUM(5:1)                               
000660     MOVE VIOS-DATUM(3:2) TO DB2-DATUM(6:2)                               
000670     MOVE '-'             TO DB2-DATUM(8:1)                               
000680     MOVE VIOS-DATUM(5:2) TO DB2-DATUM(9:2)                               
000690D    DISPLAY 'DB2-DATUM ' DB2-DATUM                                       
000700     CONTINUE.                                                            
