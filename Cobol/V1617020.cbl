000010***************************************************************           
000020 ID  DIVISION.                                                            
000030***************************************************************           
000040 PROGRAM-ID.    V1617020.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  SEPTEMBER 1991                                            
000070***************************************************************           
000080*                                                                         
000090*REMARKS:                                                                 
000100*                                                                         
000110*  * ACTION CODE N (NEW) ON EXTRACT-AUTHORITY PANEL                       
000130*                                                                         
000140*  * DB2-TABLES:                                                          
000150*                                                                         
000160*        EXTRACT_USER                                                     
000220*                                                                         
000230*  * PARMS:                                                               
000240*                                                                         
000250*        EXTRACTID X(8)                                                   
000260* USERID    X(7)                                                          
000270*                                                                         
000280*  * RETURNCODES:                                                         
000290*                                                                         
000300*         4 - FUNCTION CREATE CANCELLED                                   
000301*         8 - DUPLICATE VALUE                                             
000302*        12 - INVALID PARM                                                
000310*        16 - SEVERE ISPF-ERROR                                           
000311*        20 - SEVERE DB2-ERROR                                            
000320*                                                                         
000330***************************************************************           
000340     EJECT                                                                
000350***************************************************************           
000360 ENVIRONMENT DIVISION.                                                    
000370***************************************************************           
000380     SKIP2                                                                
000390*--------------------------------------------------------------           
000400 CONFIGURATION SECTION.                                                   
000410*--------------------------------------------------------------           
000420 SOURCE-COMPUTER. IBM-370.                                                
000430*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
000440     SKIP2                                                                
000510***************************************************************           
000520 DATA DIVISION.                                                           
000530***************************************************************           
000540     SKIP2                                                                
000640*--------------------------------------------------------------           
000650 WORKING-STORAGE SECTION.                                                 
000660*--------------------------------------------------------------           
000670 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1617020'.             
000680 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
000690 01  RETURN-CODES.                                                        
000700     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
000710     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
000720     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
000721     03  RCODE-12                 PIC S9(4) COMP SYNC VALUE 12.           
000730     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
000740     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
000741     03  RCODE-DISPL              PIC Z(4)-.                              
000750     SKIP2                                                                
000760 01  GENERAL-CONSTANTS.                                                   
000770     03 YES                       PIC X(1)  VALUE 'Y'.                    
000780     03 NOO                       PIC X(1)  VALUE 'N'.                    
000790     SKIP2                                                                
000800 01  SWITCHES.                                                            
000810     03 SW-ENTER-PRESSED          PIC X(1)  VALUE 'N'.                    
000811     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
000820     SKIP2                                                                
000821 01  W-AREAS.                                                             
000822     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
000829     SKIP2                                                                
000835 01  N-PANEL-V161702M.                                                    
000836     03 N-EXTRACTID               PIC X(8)  VALUE 'V161702A'.             
000837     03 N-USERID                  PIC X(8)  VALUE 'V161702B'.             
000839     03 N-AUTH                    PIC X(8)  VALUE 'V161702C'.             
000843     SKIP2                                                                
000848 01  LTH-PANEL-V161702M.                                                  
000849     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
000850     03 LTH-USERID                PIC S9(6) VALUE 7  COMP.                
000852     03 LTH-AUTH                  PIC S9(6) VALUE 1  COMP.                
000855     SKIP2                                                                
000860 01  ISPF-PANEL-V161702M.                                                 
000861     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
000862     03 ISPF-USERID               PIC X(7)  VALUE SPACE.                  
000864     03 ISPF-AUTH                 PIC X(1)  VALUE SPACE.                  
000867     SKIP2                                                                
000870*--------------------------------------------------------------           
000871*ISPF-CONSTANTS                                                           
000872*--------------------------------------------------------------           
000873 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
000875 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
000876 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
000877 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
000878*--------------------------------------------------------------           
000879*ISPF-FIELDS                                                              
000880*--------------------------------------------------------------           
000881 01  V161702M                     PIC X(8)  VALUE 'V1617020'.             
000882 01  ISPF-CURSOR                  PIC X(8)  VALUE SPACE.                  
000883 01  ISPF-MSG-ID                  PIC X(8)  VALUE SPACE.                  
000990     EJECT                                                                
001100*-------------------------------- DB2 ERROR HANDLING                      
001110*    -COPY V161WS                                                         
001120*++INCLUDE V161WS                                                         
001130*-------------------------------- DB2-AREAS                               
001140     EXEC SQL                                                             
001150          INCLUDE SQLCA                                                   
001160     END-EXEC.                                                            
001170     EXEC SQL                                                             
001180          INCLUDE EXTUSER                                                 
001190     END-EXEC.                                                            
001200*    -COPY EXTUSER -PRE EXTUSER-                                          
001270     EJECT                                                                
001271*--------------------------------------------------------------           
001272 LINKAGE SECTION.                                                         
001273*--------------------------------------------------------------           
001277 01  PARM-EXTRACTID         PIC X(8).                                     
001278 01  PARM-USERID            PIC X(7).                                     
001280*************************************************************             
001290 PROCEDURE DIVISION USING PARM-EXTRACTID PARM-USERID.                     
001300*************************************************************             
001314     PERFORM A-INIT                                                       
001315     IF SW-ERROR = NOO                                                    
001316       PERFORM B-CHECK-EXTUSER                                            
001317       IF SW-ERROR = NOO                                                  
001318         PERFORM C-DISPLAY-PANEL                                          
001319         IF SW-ENTER-PRESSED = YES                                        
001320           PERFORM D-INSERT-EXTUSER                                       
001321         END-IF                                                           
001340       END-IF                                                             
001350     END-IF                                                               
001420     PERFORM Z-FINIT                                                      
001430     GOBACK                                                               
001440     CONTINUE.                                                            
001450                                                                          
001460*--------------------------------------------------------------           
001470 A-INIT SECTION.                                                          
001480*--------------------------------------------------------------           
001490     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
001500D    DISPLAY ABEND-SECTION                                                
001510     SKIP2                                                                
001511     MOVE NOO TO SW-ERROR                                                 
001512     PERFORM AA-INIT-PARMS                                                
001523     PERFORM AB-VDEF-PANEL-V161702M                                       
001540     CONTINUE.                                                            
001550     EJECT                                                                
001560*--------------------------------------------------------------           
001570 AA-INIT-PARMS SECTION.                                                   
001580*--------------------------------------------------------------           
001590     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
001600D    DISPLAY ABEND-SECTION                                                
001610     SKIP2                                                                
001630D    DISPLAY 'PARM-EXTRACTID    :' PARM-EXTRACTID                         
001631D    DISPLAY 'PARM-USERID       :' PARM-USERID                            
001640     IF PARM-EXTRACTID = SPACE                                            
001650        MOVE RCODE-12 TO RCODE                                            
001660        MOVE YES TO SW-ERROR                                              
001661     ELSE                                                                 
001662       IF PARM-USERID = SPACE                                             
001663          MOVE RCODE-12 TO RCODE                                          
001664          MOVE YES TO SW-ERROR                                            
001665       END-IF                                                             
001666     END-IF                                                               
001667     CONTINUE.                                                            
001668     EJECT                                                                
001694*--------------------------------------------------------------           
001695 AB-VDEF-PANEL-V161702M SECTION.                                          
001696*--------------------------------------------------------------           
001697     MOVE 'AB-VDEF-PANEL-V161702M    ' TO ABEND-SECTION                   
001698D    DISPLAY ABEND-SECTION                                                
001699     SKIP2                                                                
001700     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID ISPF-EXTRACTID              
001701                          CHAR LTH-EXTRACTID                              
001707     CALL 'ISPLINK' USING VDEFINE N-USERID ISPF-USERID                    
001708                          CHAR LTH-USERID                                 
001721     CALL 'ISPLINK' USING VDEFINE N-AUTH                                  
001722                                  ISPF-AUTH                               
001723                                  CHAR                                    
001724                                  LTH-AUTH                                
001741                                                                          
001742     CONTINUE.                                                            
001750     EJECT                                                                
001751*--------------------------------------------------------------           
001752 B-CHECK-EXTUSER SECTION.                                                 
001753*--------------------------------------------------------------           
001754     MOVE 'B-CHECK-EXTUSER' TO ABEND-SECTION                              
001755D    DISPLAY ABEND-SECTION                                                
001756     SKIP2                                                                
001757     MOVE PARM-EXTRACTID TO EXTUSER-EXTRACTID                             
001758     MOVE PARM-USERID    TO EXTUSER-USERID                                
001759     PERFORM SQL-SELECT-EXTUSER                                           
001760     IF SQLCODE = +000                                                    
001761        MOVE YES TO SW-ERROR                                              
001762        MOVE RCODE-8 TO RCODE                                             
001763     END-IF                                                               
001764     CONTINUE.                                                            
001770     EJECT                                                                
001780*--------------------------------------------------------------           
001781 C-DISPLAY-PANEL SECTION.                                                 
001782*--------------------------------------------------------------           
001790     MOVE 'C-DISPLAY-PANEL         ' TO ABEND-SECTION                     
001800D    DISPLAY ABEND-SECTION                                                
001900     SKIP2                                                                
001901     PERFORM SQL-COMMIT                                                   
001902     MOVE N-AUTH          TO ISPF-CURSOR                                  
001910     MOVE PARM-EXTRACTID TO ISPF-EXTRACTID                                
001911     MOVE PARM-USERID    TO ISPF-USERID                                   
001920     PERFORM S01-DISPLAY-PANEL                                            
001921     IF RCODE = 0                                                         
001922        MOVE YES TO SW-ENTER-PRESSED                                      
001923     ELSE                                                                 
001924        MOVE NOO TO SW-ENTER-PRESSED                                      
001925        IF RCODE = 8                                                      
001926           MOVE RCODE-4 TO RCODE                                          
001927        END-IF                                                            
001928     END-IF                                                               
001959     CONTINUE.                                                            
001960     EJECT                                                                
001961*--------------------------------------------------------------           
001962 D-INSERT-EXTUSER SECTION.                                                
001963*--------------------------------------------------------------           
001964     MOVE 'D-INSERT-EXTUSER      ' TO ABEND-SECTION                       
001965D    DISPLAY ABEND-SECTION                                                
001966     SKIP2                                                                
001967     MOVE ISPF-EXTRACTID     TO EXTUSER-EXTRACTID                         
001968     MOVE ISPF-USERID        TO EXTUSER-USERID                            
001973     MOVE ISPF-AUTH          TO EXTUSER-AUTH                              
001979     PERFORM SQL-INSERT-EXTUSER                                           
001980     IF SQLCODE = +000                                                    
001981        PERFORM SQL-COMMIT                                                
001982     ELSE                                                                 
001985        MOVE YES TO SW-ERROR                                              
001986        IF SQLCODE = -803                                                 
001987           MOVE RCODE-8 TO RCODE                                          
001988        END-IF                                                            
001989     END-IF                                                               
001990                                                                          
001991     CONTINUE.                                                            
001992     EJECT                                                                
002027*--------------------------------------------------------------           
002028 Z-FINIT SECTION.                                                         
002029*--------------------------------------------------------------           
002030     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
002031D    DISPLAY ABEND-SECTION                                                
002032     SKIP2                                                                
002033     PERFORM ZA-VDEL-PANEL-V161702M                                       
002034     MOVE RCODE TO RETURN-CODE                                            
002040     CONTINUE.                                                            
002050     EJECT                                                                
003010*--------------------------------------------------------------           
003020 ZA-VDEL-PANEL-V161702M SECTION.                                          
003030*--------------------------------------------------------------           
003031     MOVE 'ZB-VDEL-PANEL-V161702M    ' TO ABEND-SECTION                   
003032D    DISPLAY ABEND-SECTION                                                
003033     SKIP2                                                                
003034     CALL 'ISPLINK' USING VDELETE N-EXTRACTID                             
003036     CALL 'ISPLINK' USING VDELETE N-USERID                                
003040     CALL 'ISPLINK' USING VDELETE N-AUTH                                  
003046     CONTINUE.                                                            
003047     EJECT                                                                
003048*--------------------------------------------------------------           
003049 S01-DISPLAY-PANEL SECTION.                                               
003050*--------------------------------------------------------------           
003051     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
003052D    DISPLAY ABEND-SECTION                                                
003053     SKIP2                                                                
003054     CALL 'ISPLINK' USING DISPLAYE V161702M                               
003055                          ISPF-MSG-ID ISPF-CURSOR                         
003056     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
003057     IF RCODE > +12                                                       
003058        PERFORM S99-ERROR-ROUTINE                                         
003059        MOVE YES TO SW-ERROR                                              
003060        MOVE RCODE-16 TO RCODE                                            
003070     END-IF                                                               
003075     CONTINUE.                                                            
003076     EJECT                                                                
003092*--------------------------------------------------------------           
003093 S99-ERROR-ROUTINE SECTION.                                               
003094*--------------------------------------------------------------           
003095     SKIP2                                                                
003096     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
003097     RCODE-DISPL DELIMITED BY SIZE                                        
003098     INTO W-ERROR-MESSAGE                                                 
003099     DISPLAY W-ERROR-MESSAGE                                              
003100     CONTINUE.                                                            
003102     EJECT                                                                
003103*--------------------------------------------------------------           
003104 SQL-SELECT-EXTUSER SECTION.                                              
003105*--------------------------------------------------------------           
003106     MOVE 'SQL-SELECT-EXTUSER     ' TO ABEND-SECTION                      
003107D    DISPLAY ABEND-SECTION                                                
003108     SKIP2                                                                
003109     EXEC SQL                                                             
003110        SELECT  EXTRACTID                                                 
003111        INTO :EXTUSER-EXTRACTID                                           
003112        FROM EXTRACT_USER                                                 
003113        WHERE EXTRACTID = :EXTUSER-EXTRACTID                              
003114        AND   USERID    = :EXTUSER-USERID                                 
003116     END-EXEC                                                             
003117     PERFORM S95-CONTROL-SQLCODE                                          
003118     CONTINUE.                                                            
003119     EJECT                                                                
003120*--------------------------------------------------------------           
003121 SQL-INSERT-EXTUSER SECTION.                                              
003122*--------------------------------------------------------------           
003123     MOVE 'SQL-INSERT-EXTUSER        ' TO ABEND-SECTION                   
003124D    DISPLAY ABEND-SECTION                                                
003125     SKIP2                                                                
003126     EXEC SQL                                                             
003127        INSERT  INTO EXTRACT_USER (EXTRACTID,                             
003128                                   USERID,                                
003130                                   AUTH)                                  
003134        VALUES                   (:EXTUSER-EXTRACTID,                     
003135                                  :EXTUSER-USERID,                        
003137                                  :EXTUSER-AUTH)                          
003142     END-EXEC                                                             
003146     PERFORM S95-CONTROL-SQLCODE                                          
003147     CONTINUE.                                                            
003148     EJECT                                                                
003149*--------------------------------------------------------------           
003150 SQL-COMMIT SECTION.                                                      
003151*--------------------------------------------------------------           
003152     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
003153D    DISPLAY ABEND-SECTION                                                
003154     SKIP2                                                                
003155     EXEC SQL                                                             
003156         COMMIT                                                           
003157     END-EXEC                                                             
003158     PERFORM S95-CONTROL-SQLCODE                                          
003159     CONTINUE.                                                            
003160     EJECT                                                                
003161*-------------------------------- DB2 ERROR HANDLING                      
003170*    -COPY V161PS                                                         
003200*++INCLUDE V161PS                                                         
