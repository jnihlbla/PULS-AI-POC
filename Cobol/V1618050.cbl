000010***************************************************************           
000020 ID  DIVISION.                                                            
000030***************************************************************           
000040 PROGRAM-ID.    V1618050.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  SEPTEMBER 1991                                            
000070***************************************************************           
000080*                                                                         
000090*REMARKS:                                                                 
000100*                                                                         
000110*  * ACTION CODE D (DELETE) ON MASTER AUTHORITY PANEL                     
000130*                                                                         
000140*  * DB2-TABLES:                                                          
000150*                                                                         
000160*        EXTRACT_MASTER                                                   
000220*                                                                         
000230*  * PARMS:                                                               
000240*                                                                         
000250*        USERID X(7).                                                     
000260*                                                                         
000270*                                                                         
000280*  * RETURNCODES:                                                         
000290*                                                                         
000300*         4 - FUNCTION DELETE CANCELLED                                   
000301*         8 - ROW NOT FOUND                                               
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
000670 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1618050'.             
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
000835 01  N-PANEL-V161805M.                                                    
000836     03 N-USERID                  PIC X(8)  VALUE 'V161805A'.             
000839     03 N-AUTH                    PIC X(8)  VALUE 'V161805B'.             
000840     03 N-PREFIX                  PIC X(8)  VALUE 'V161805C'.             
000845     SKIP2                                                                
000851 01  LTH-PANEL-V161805M.                                                  
000853     03 LTH-USERID                PIC S9(6) VALUE 7  COMP.                
000855     03 LTH-AUTH                  PIC S9(6) VALUE 1  COMP.                
000856     03 LTH-PREFIX                PIC S9(6) VALUE 8  COMP.                
000861     SKIP2                                                                
000866 01  ISPF-PANEL-V161805M.                                                 
000868     03 ISPF-USERID               PIC X(7)  VALUE SPACE.                  
000870     03 ISPF-AUTH                 PIC X(1)  VALUE SPACE.                  
000871     03 ISPF-PREFIX               PIC X(8)  VALUE SPACE.                  
000876     SKIP2                                                                
000879*--------------------------------------------------------------           
000880*ISPF-CONSTANTS                                                           
000881*--------------------------------------------------------------           
000882 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
000883 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
000884 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
000885 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
000886*--------------------------------------------------------------           
000887*ISPF-FIELDS                                                              
000888*--------------------------------------------------------------           
000889 01  V161805M                     PIC X(8)  VALUE 'V1618050'.             
000890 01  ISPF-CURSOR                  PIC X(8)  VALUE SPACE.                  
000891 01  ISPF-MSG-ID                  PIC X(8)  VALUE SPACE.                  
000990     EJECT                                                                
001100*-------------------------------- DB2 ERROR HANDLING                      
001110*    -COPY V161WS                                                         
001120*++INCLUDE V161WS                                                         
001130*-------------------------------- DB2-AREAS                               
001140     EXEC SQL                                                             
001150          INCLUDE SQLCA                                                   
001160     END-EXEC.                                                            
001170     EXEC SQL                                                             
001180          INCLUDE EXTMAST                                                 
001190     END-EXEC.                                                            
001200*    -COPY EXTMAST -PRE EXTMAST-                                          
001271     EJECT                                                                
001272*--------------------------------------------------------------           
001273 LINKAGE SECTION.                                                         
001274*--------------------------------------------------------------           
001278 01  PARM-USERID            PIC X(7).                                     
001280*************************************************************             
001290 PROCEDURE DIVISION USING PARM-USERID.                                    
001300*************************************************************             
001310     PERFORM A-INIT                                                       
001311     IF SW-ERROR = NOO                                                    
001312       PERFORM B-SELECT-PANELINFO                                         
001313       IF SW-ERROR = NOO                                                  
001314          PERFORM C-DISPLAY-PANEL                                         
001315          IF SW-ENTER-PRESSED = YES                                       
001316            PERFORM D-DELETE-EXTMAST                                      
001319          END-IF                                                          
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
001523     PERFORM AB-VDEF-PANEL-V161805M                                       
001540     CONTINUE.                                                            
001550     EJECT                                                                
001560*--------------------------------------------------------------           
001570 AA-INIT-PARMS    SECTION.                                                
001580*--------------------------------------------------------------           
001590     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
001600D    DISPLAY ABEND-SECTION                                                
001610     SKIP2                                                                
001631D    DISPLAY 'PARM-USERID       :' PARM-USERID                            
001636     IF PARM-USERID = SPACE                                               
001637        MOVE RCODE-12 TO RCODE                                            
001638        MOVE YES TO SW-ERROR                                              
001639     END-IF                                                               
001643     CONTINUE.                                                            
001650     EJECT                                                                
001694*--------------------------------------------------------------           
001695 AB-VDEF-PANEL-V161805M SECTION.                                          
001696*--------------------------------------------------------------           
001697     MOVE 'AB-VDEF-PANEL-V161805M    ' TO ABEND-SECTION                   
001698D    DISPLAY ABEND-SECTION                                                
001699     SKIP2                                                                
001702     CALL 'ISPLINK' USING VDEFINE N-USERID ISPF-USERID                    
001703                          CHAR LTH-USERID                                 
001704     CALL 'ISPLINK' USING VDEFINE N-PREFIX ISPF-PREFIX                    
001705                          CHAR LTH-PREFIX                                 
001706     CALL 'ISPLINK' USING VDEFINE N-AUTH                                  
001707                                  ISPF-AUTH                               
001708                                  CHAR LTH-AUTH                           
001741                                                                          
001742     CONTINUE.                                                            
001750     EJECT                                                                
001751*--------------------------------------------------------------           
001752 B-SELECT-PANELINFO SECTION.                                              
001753*--------------------------------------------------------------           
001754     MOVE 'B-SELECT-PANELINFO      ' TO ABEND-SECTION                     
001755D    DISPLAY ABEND-SECTION                                                
001756     SKIP2                                                                
001758     MOVE PARM-USERID        TO EXTMAST-USERID                            
001759     PERFORM SQL-SELECT-EXTMAST                                           
001760     IF SQLCODE NOT = +000                                                
001761        MOVE YES TO SW-ERROR                                              
001762        IF SQLCODE = +100                                                 
001763          MOVE RCODE-8 TO RCODE                                           
001764        END-IF                                                            
001765     END-IF                                                               
001766     CONTINUE.                                                            
001770     EJECT                                                                
001961*--------------------------------------------------------------           
001962 C-DISPLAY-PANEL SECTION.                                                 
001963*--------------------------------------------------------------           
001964     MOVE 'C-DISPLAY-PANEL         ' TO ABEND-SECTION                     
001965D    DISPLAY ABEND-SECTION                                                
001966     SKIP2                                                                
001968     MOVE PARM-USERID           TO ISPF-USERID                            
001969     MOVE EXTMAST-PREFIX        TO ISPF-PREFIX                            
001970     MOVE EXTMAST-AUTH TO ISPF-AUTH                                       
001979     PERFORM S01-DISPLAY-PANEL                                            
001980     IF RCODE = 0                                                         
001981        MOVE YES TO SW-ENTER-PRESSED                                      
001982     ELSE                                                                 
001983        MOVE NOO TO SW-ENTER-PRESSED                                      
001984        IF RCODE = 8                                                      
001985           MOVE RCODE-4 TO RCODE                                          
001986        END-IF                                                            
001987     END-IF                                                               
001988     CONTINUE.                                                            
001989     EJECT                                                                
001990*--------------------------------------------------------------           
001991 D-DELETE-EXTMAST SECTION.                                                
001992*--------------------------------------------------------------           
001993     MOVE 'D-DELETE-EXTMAST        ' TO ABEND-SECTION                     
002000D    DISPLAY ABEND-SECTION                                                
002010     SKIP2                                                                
002021     MOVE PARM-USERID        TO EXTMAST-USERID                            
002030     PERFORM SQL-DELETE-EXTMAST                                           
002031     IF SQLCODE NOT = +000                                                
002032        MOVE YES TO SW-ERROR                                              
002033        IF SQLCODE = +100                                                 
002034           MOVE RCODE-8 TO RCODE                                          
002035        END-IF                                                            
002036     END-IF                                                               
002037     CONTINUE.                                                            
002038     EJECT                                                                
002040*--------------------------------------------------------------           
002041 Z-FINIT SECTION.                                                         
002042*--------------------------------------------------------------           
002043     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
002044D    DISPLAY ABEND-SECTION                                                
002045     SKIP2                                                                
002047     PERFORM ZA-VDEL-PANEL-V161805M                                       
002048     MOVE RCODE TO RETURN-CODE                                            
002049     CONTINUE.                                                            
002050     EJECT                                                                
003010*--------------------------------------------------------------           
003020 ZA-VDEL-PANEL-V161805M SECTION.                                          
003030*--------------------------------------------------------------           
003031     MOVE 'ZA-VDEL-PANEL-V161805M    ' TO ABEND-SECTION                   
003032D    DISPLAY ABEND-SECTION                                                
003033     SKIP2                                                                
003034     CALL 'ISPLINK' USING VDELETE N-USERID                                
003035     CALL 'ISPLINK' USING VDELETE N-PREFIX                                
003036     CALL 'ISPLINK' USING VDELETE N-AUTH                                  
003046     CONTINUE.                                                            
003047     EJECT                                                                
003048*--------------------------------------------------------------           
003049 S01-DISPLAY-PANEL SECTION.                                               
003050*--------------------------------------------------------------           
003051     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
003052D    DISPLAY ABEND-SECTION                                                
003053     SKIP2                                                                
003054     CALL 'ISPLINK' USING DISPLAYE V161805M                               
003055                          ISPF-MSG-ID ISPF-CURSOR                         
003056     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
003057     IF RCODE > +12                                                       
003058        PERFORM S99-ERROR-ROUTINE                                         
003059        MOVE YES TO SW-ERROR                                              
003060        MOVE RCODE-16 TO RCODE                                            
003070     END-IF                                                               
003075     CONTINUE.                                                            
003076     EJECT                                                                
003077*--------------------------------------------------------------           
003078 S99-ERROR-ROUTINE SECTION.                                               
003079*--------------------------------------------------------------           
003082     SKIP2                                                                
003083     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
003084     RCODE-DISPL DELIMITED BY SIZE                                        
003085     INTO W-ERROR-MESSAGE                                                 
003086     DISPLAY W-ERROR-MESSAGE                                              
003087     CONTINUE.                                                            
003102     EJECT                                                                
003103*--------------------------------------------------------------           
003104 SQL-SELECT-EXTMAST SECTION.                                              
003105*--------------------------------------------------------------           
003106     MOVE 'SQL-SELECT-EXTMAST          ' TO ABEND-SECTION                 
003107D    DISPLAY ABEND-SECTION                                                
003108     SKIP2                                                                
003109     EXEC SQL                                                             
003110        SELECT  AUTH, PREFIX                                              
003111        INTO :EXTMAST-AUTH, :EXTMAST-PREFIX                               
003112        FROM EXTRACT_MASTER                                               
003113        WHERE USERID = :EXTMAST-USERID                                    
003116     END-EXEC                                                             
003117     PERFORM S95-CONTROL-SQLCODE                                          
003118     CONTINUE.                                                            
003120     EJECT                                                                
003140*--------------------------------------------------------------           
003150 SQL-DELETE-EXTMAST SECTION.                                              
003160*--------------------------------------------------------------           
003161     MOVE 'SQL-DELETE-EXTMAST        ' TO ABEND-SECTION                   
003162D    DISPLAY ABEND-SECTION                                                
003163     SKIP2                                                                
003164     EXEC SQL                                                             
003165        DELETE FROM EXTRACT_MASTER                                        
003168        WHERE USERID = :EXTMAST-USERID                                    
003171     END-EXEC                                                             
003172     PERFORM S95-CONTROL-SQLCODE                                          
003173     CONTINUE.                                                            
003174     EJECT                                                                
003175*-------------------------------- DB2 FELHANTERING SEKTION                
003180*    -COPY V161PS                                                         
003200*++INCLUDE V161PS                                                         
