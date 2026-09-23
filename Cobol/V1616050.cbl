000010***************************************************************           
000020 ID  DIVISION.                                                            
000030***************************************************************           
000040 PROGRAM-ID.    V1616050.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  SEPTEMBER 1991                                            
000070***************************************************************           
000080*                                                                         
000090*REMARKS:                                                                 
000100*                                                                         
000110*  * ACTION CODE D (DELETE) ON EXTRACTADM PANEL                           
000130*                                                                         
000140*  * DB2-TABLES:                                                          
000150*                                                                         
000160*        EXTRACT                                                          
000220*                                                                         
000230*  * SUBPROGRAMS:                                                         
000240*                                                                         
000250*        V1619400                                                         
000260*                                                                         
000261*  * PARMS:                                                               
000262*                                                                         
000263*        EXTRACTID X(8)                                                   
000264*                                                                         
000280*  * RETURNCODES:                                                         
000290*                                                                         
000300*         4 - FUNCTION DELETE CANCELLED                                   
000301*         8 - NO AUTHORITY FOR DELETE                                     
000302*        12 - INVALID PARMS                                               
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
000670 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1616050'.             
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
000812     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
000820     SKIP2                                                                
000821 01  W-AREAS.                                                             
000822     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
000829     SKIP2                                                                
000835 01  N-PANEL-V161605M.                                                    
000836     03 N-EXTRACTID               PIC X(8)  VALUE 'V161605A'.             
000837     03 N-DESCRIPTION             PIC X(8)  VALUE 'V161605B'.             
000845     SKIP2                                                                
000851 01  LTH-PANEL-V161605M.                                                  
000852     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
000853     03 LTH-DESCRIPTION           PIC S9(6) VALUE 25 COMP.                
000861     SKIP2                                                                
000866 01  ISPF-PANEL-V161605M.                                                 
000867     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
000868     03 ISPF-DESCRIPTION          PIC X(25) VALUE SPACE.                  
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
000889 01  V161605M                     PIC X(8)  VALUE 'V1616050'.             
000890 01  ISPF-CURSOR                  PIC X(8)  VALUE SPACE.                  
000891 01  ISPF-MSG-ID                  PIC X(8)  VALUE SPACE.                  
000892 01  DYNAMIC-SUBPROGRAMS.                                                 
000900     03 V16194                    PIC X(8)  VALUE 'V16194  '.             
000990     EJECT                                                                
001100*-------------------------------- DB2 FELHANTERING                        
001110*    -COPY V161WS                                                         
001120*++INCLUDE V161WS                                                         
001130*-------------------------------- DB2-AREOR                               
001140     EXEC SQL                                                             
001150          INCLUDE SQLCA                                                   
001160     END-EXEC.                                                            
001170     EXEC SQL                                                             
001180          INCLUDE EXTRACT                                                 
001190     END-EXEC.                                                            
001200*    -COPY EXTRACT -PRE EXTRACT-                                          
001271     EJECT                                                                
001272*--------------------------------------------------------------           
001273 LINKAGE SECTION.                                                         
001274*--------------------------------------------------------------           
001275 01 PARM.                                                                 
001277     03 PARM-EXTRACTID         PIC X(8).                                  
001280*************************************************************             
001290 PROCEDURE DIVISION USING PARM.                                           
001300*************************************************************             
001310     PERFORM A-INIT                                                       
001311     IF SW-ERROR = NOO                                                    
001312       PERFORM B-CHECK-AUTHORIZATION                                      
001313       IF SW-ERROR = NOO                                                  
001314         PERFORM C-SELECT-PANELINFO                                       
001315         IF SW-ERROR = NOO                                                
001316            PERFORM D-DISPLAY-PANEL                                       
001317            IF SW-ENTER-PRESSED = YES                                     
001318              PERFORM E-DELETE-EXTRACT                                    
001319            END-IF                                                        
001330         END-IF                                                           
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
001523     PERFORM AB-VDEF-PANEL-V161605M                                       
001540     CONTINUE.                                                            
001550     EJECT                                                                
001560*--------------------------------------------------------------           
001570 AA-INIT-PARMS    SECTION.                                                
001580*--------------------------------------------------------------           
001590     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
001600D    DISPLAY ABEND-SECTION                                                
001610     SKIP2                                                                
001630D    DISPLAY 'PARM-EXTRACTID    :' PARM-EXTRACTID                         
001631     IF PARM-EXTRACTID = SPACE                                            
001632        MOVE RCODE-12 TO RCODE                                            
001633        MOVE YES TO SW-ERROR                                              
001634     END-IF                                                               
001640     CONTINUE.                                                            
001650     EJECT                                                                
001694*--------------------------------------------------------------           
001695 AB-VDEF-PANEL-V161605M SECTION.                                          
001696*--------------------------------------------------------------           
001697     MOVE 'AB-VDEF-PANEL-V161605M    ' TO ABEND-SECTION                   
001698D    DISPLAY ABEND-SECTION                                                
001699     SKIP2                                                                
001700     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID ISPF-EXTRACTID              
001701                          CHAR LTH-EXTRACTID                              
001702     CALL 'ISPLINK' USING VDEFINE N-DESCRIPTION ISPF-DESCRIPTION          
001703                          CHAR LTH-DESCRIPTION                            
001741                                                                          
001742     CONTINUE.                                                            
001750     EJECT                                                                
001751*--------------------------------------------------------------           
001752 B-CHECK-AUTHORIZATION SECTION.                                           
001753*--------------------------------------------------------------           
001754     MOVE 'B-CHECK-AUTHORIZATION  ' TO ABEND-SECTION                      
001755D    DISPLAY ABEND-SECTION                                                
001756     SKIP2                                                                
001757     CALL V16194 USING PARM-EXTRACTID                                     
001759     IF RETURN-CODE = 0                                                   
001761        MOVE NOO TO SW-ERROR                                              
001762     ELSE                                                                 
001764        MOVE RCODE-8 TO RCODE                                             
001765        MOVE YES TO SW-ERROR                                              
001766     END-IF                                                               
001767     CONTINUE.                                                            
001768     EJECT                                                                
001769*--------------------------------------------------------------           
001770 C-SELECT-PANELINFO SECTION.                                              
001780*--------------------------------------------------------------           
001790     MOVE 'C-SELECT-PANELINFO      ' TO ABEND-SECTION                     
001800D    DISPLAY ABEND-SECTION                                                
001900     SKIP2                                                                
001910     MOVE PARM-EXTRACTID     TO EXTRACT-EXTRACTID                         
001920     PERFORM SQL-SELECT-EXTRACT                                           
001930     IF SQLCODE NOT = +000                                                
001931        MOVE YES TO SW-ERROR                                              
001932        IF SQLCODE = +100                                                 
001933          MOVE RCODE-8 TO RCODE                                           
001934        END-IF                                                            
001951     END-IF                                                               
001959     CONTINUE.                                                            
001960     EJECT                                                                
001961*--------------------------------------------------------------           
001962 D-DISPLAY-PANEL SECTION.                                                 
001963*--------------------------------------------------------------           
001964     MOVE 'D-DISPLAY-PANEL         ' TO ABEND-SECTION                     
001965D    DISPLAY ABEND-SECTION                                                
001966     SKIP2                                                                
001968     MOVE EXTRACT-EXTRACTID   TO ISPF-EXTRACTID                           
001969     MOVE EXTRACT-DESCRIPTION TO ISPF-DESCRIPTION                         
001970     PERFORM SQL-COMMIT                                                   
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
001991 E-DELETE-EXTRACT SECTION.                                                
001992*--------------------------------------------------------------           
001993     MOVE 'E-DELETE-EXTRACT        ' TO ABEND-SECTION                     
002000D    DISPLAY ABEND-SECTION                                                
002010     SKIP2                                                                
002020     MOVE PARM-EXTRACTID     TO EXTRACT-EXTRACTID                         
002030     PERFORM SQL-DELETE-EXTRACTID                                         
002031     IF SQLCODE NOT = +000                                                
002032        MOVE YES TO SW-ERROR                                              
002033        IF SQLCODE = +100                                                 
002034           MOVE RCODE-8 TO RCODE                                          
002035        END-IF                                                            
002036     END-IF                                                               
002038     CONTINUE.                                                            
002039     EJECT                                                                
002065*--------------------------------------------------------------           
002066 Z-FINIT SECTION.                                                         
002067*--------------------------------------------------------------           
002068     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
002069D    DISPLAY ABEND-SECTION                                                
002070     SKIP2                                                                
002071     PERFORM ZA-VDEL-PANEL-V161605M                                       
002072     MOVE RCODE TO RETURN-CODE                                            
002073     CONTINUE.                                                            
002080     EJECT                                                                
003010*--------------------------------------------------------------           
003020 ZA-VDEL-PANEL-V161605M SECTION.                                          
003030*--------------------------------------------------------------           
003031     MOVE 'ZA-VDEL-PANEL-V161605M    ' TO ABEND-SECTION                   
003032D    DISPLAY ABEND-SECTION                                                
003033     SKIP2                                                                
003034     CALL 'ISPLINK' USING VDELETE N-EXTRACTID                             
003035     CALL 'ISPLINK' USING VDELETE N-DESCRIPTION                           
003046     CONTINUE.                                                            
003047     EJECT                                                                
003048*--------------------------------------------------------------           
003049 S01-DISPLAY-PANEL SECTION.                                               
003050*--------------------------------------------------------------           
003051     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
003052D    DISPLAY ABEND-SECTION                                                
003053     SKIP2                                                                
003054     CALL 'ISPLINK' USING DISPLAYE V161605M                               
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
003104 SQL-SELECT-EXTRACT SECTION.                                              
003105*--------------------------------------------------------------           
003106     MOVE 'SQL-SELECT-EXTRACT          ' TO ABEND-SECTION                 
003107D    DISPLAY ABEND-SECTION                                                
003108     SKIP2                                                                
003109     EXEC SQL                                                             
003110        SELECT  DESCRIPTION                                               
003113        INTO :EXTRACT-DESCRIPTION                                         
003121        FROM EXTRACT                                                      
003122        WHERE EXTRACTID = :EXTRACT-EXTRACTID                              
003124     END-EXEC                                                             
003128     PERFORM S95-CONTROL-SQLCODE                                          
003129     CONTINUE.                                                            
003130     EJECT                                                                
003140*--------------------------------------------------------------           
003150 SQL-DELETE-EXTRACTID SECTION.                                            
003160*--------------------------------------------------------------           
003161     MOVE 'SQL-DELETE-EXTRACTID        ' TO ABEND-SECTION                 
003162D    DISPLAY ABEND-SECTION                                                
003163     SKIP2                                                                
003164     EXEC SQL                                                             
003165        DELETE FROM EXTRACT                                               
003168        WHERE EXTRACTID = :EXTRACT-EXTRACTID                              
003170     END-EXEC                                                             
003171     PERFORM S95-CONTROL-SQLCODE                                          
003172     CONTINUE.                                                            
003173     EJECT                                                                
003174*--------------------------------------------------------------           
003175 SQL-COMMIT SECTION.                                                      
003176*--------------------------------------------------------------           
003177     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
003178D    DISPLAY ABEND-SECTION                                                
003179     SKIP2                                                                
003180     EXEC SQL                                                             
003181        COMMIT                                                            
003183     END-EXEC                                                             
003184     PERFORM S95-CONTROL-SQLCODE                                          
003185     CONTINUE.                                                            
003186     EJECT                                                                
003187*-------------------------------- DB2 ERROR HANDLING                      
003190*    -COPY V161PS                                                         
003200*++INCLUDE V161PS                                                         
