000010*************************************************************             
000020 ID  DIVISION.                                                            
000021*************************************************************             
000040 PROGRAM-ID.    V1619200.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  OKTOBER 1990                                              
000101***************************************************************           
000102*                                                                         
000103*REMARKS:                                                                 
000104*                                                                         
000105*  * FÖRVANDLAR FRÅN YYYY-MM-DD (DB2-FORMAT) TILL YYMMDD (VIOS-FOR        
000106*                                                                         
000107*  * PARAMETRAR:                                                          
000108*                                                                         
000110*        DB2-DATUM                                                        
000111*        VIOS-DATUM                                                       
000112*                                                                         
000113*  * RETURKODER:                                                          
000114*                                                                         
000115*                                                                         
000116***************************************************************           
000120 ENVIRONMENT DIVISION.                                                    
000121*************************************************************             
000140 CONFIGURATION SECTION.                                                   
000150 SOURCE-COMPUTER. IBM-370.                                                
000160*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
000170                                                                          
000171*************************************************************             
000190 DATA DIVISION.                                                           
000191*************************************************************             
000210 WORKING-STORAGE SECTION.                                                 
000220 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1619200'.             
000230 01  ABENDANDE-SECTION            PIC X(25) VALUE SPACE.                  
000240 01  RETUR-KODER.                                                         
000250     03  RKOD                     PIC S9(4) COMP SYNC VALUE ZERO.         
000260     03  RKOD-4                   PIC S9(4) COMP SYNC VALUE 4.            
000270     03  RKOD-8                   PIC S9(4) COMP SYNC VALUE 8.            
000280     03  RKOD-16                  PIC S9(4) COMP SYNC VALUE 16.           
000290     03  RKOD-20                  PIC S9(4) COMP SYNC VALUE 20.           
000300     EJECT                                                                
000310*-------------------------------- PARM-AREOR                              
000320 LINKAGE SECTION.                                                         
000330 01  PARM.                                                                
000340     03  DB2-DATUM            PIC X(10).                                  
000350     03  VIOS-DATUM           PIC X(6).                                   
000360     EJECT                                                                
000370                                                                          
000380                                                                          
000390*************************************************************             
000400 PROCEDURE DIVISION USING PARM.                                           
000410*************************************************************             
000420     PERFORM A-INIT                                                       
000430     PERFORM B-KONV-TILL-VIOS-FORMAT                                      
000440     GOBACK                                                               
000450     CONTINUE.                                                            
000460                                                                          
000470 A-INIT SECTION.                                                          
000480     MOVE 'A-INIT                  ' TO ABENDANDE-SECTION                 
000490D    DISPLAY ABENDANDE-SECTION                                            
000500                                                                          
000510D    DISPLAY 'PARAMETRAR '                                                
000520D    DISPLAY DB2-DATUM                                                    
000530     CONTINUE.                                                            
000540                                                                          
000550 B-KONV-TILL-VIOS-FORMAT SECTION.                                         
000560     MOVE 'B-KONV-TILL-VIOS-FORMAT ' TO ABENDANDE-SECTION                 
000570D    DISPLAY ABENDANDE-SECTION                                            
000580                                                                          
000590     IF DB2-DATUM = SPACE THEN                                            
000600        ACCEPT VIOS-DATUM FROM DATE                                       
000610     ELSE                                                                 
000620       MOVE DB2-DATUM(3:2) TO VIOS-DATUM(1:2)                             
000630       MOVE DB2-DATUM(6:2) TO VIOS-DATUM(3:2)                             
000640       MOVE DB2-DATUM(9:2) TO VIOS-DATUM(5:2)                             
000650     END-IF                                                               
000660D    DISPLAY 'VIOS-DATUM ' VIOS-DATUM                                     
000670     CONTINUE.                                                            
