000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W0118300.                                                
000003 AUTHOR.         PRIYA RC.                                                
000004 DATE-WRITTEN.   02/09/24.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*                                                                         
000008*    FUNCTION: CREATE FILES TO AZURE DATALAKE IN DISPLAY FORMAT           
000009*              -NDC PROCUREMENT DATA (WDK723 EXTRACT)                     
000010*    ABENDCODES:                                                          
000011*                                                                         
000012                                                                          
000013     SKIP3                                                                
000014 ENVIRONMENT DIVISION.                                                    
000015     SKIP2                                                                
000016 INPUT-OUTPUT SECTION.                                                    
000017                                                                          
000018 FILE-CONTROL.                                                            
000019     SKIP2                                                                
000022*          --- INPUT FILE FROM W01184 - WDK723                            
000023     SELECT W01183                     ASSIGN TO W01183D1.                
000026*          --- OUTPUT FILE TO AZURE DATALAKE - WDK723                     
000027     SELECT W01183X                    ASSIGN TO W01183D2.                
000028     EJECT                                                                
000029 DATA DIVISION.                                                           
000030     SKIP3                                                                
000031 FILE SECTION.                                                            
000032     SKIP3                                                                
000039 FD  W01183                                                               
000040     RECORDING       F                                                    
000041     BLOCK CONTAINS  0.                                                   
000042                                                                          
000043*01  -COPY W01183          -PRE  IN1-   -L.                               
000044     SKIP3                                                                
000050     SKIP3                                                                
000051 FD  W01183X                                                              
000052     RECORDING       F                                                    
000053     BLOCK CONTAINS  0.                                                   
000054                                                                          
000055*01  RECORD -COPY W01183X   -PRE  OUTX1- -L.                              
000056     EJECT                                                                
000057 WORKING-STORAGE SECTION.                                                 
000058                                                                          
000059 77  IDPGM                       PIC X(8)    VALUE 'W0118300'.            
000060 77  YES                         PIC X       VALUE 'J'.                   
000061 77  NOO                         PIC X       VALUE 'N'.                   
000062 77  IX1                         PIC S9(9)   VALUE +0 COMP SYNC.          
000063                                                                          
000067 77  W01183-EOF-SW               PIC X       VALUE 'N'.                   
000068     88  END-OF-W01183                       VALUE 'J'.                   
000069     EJECT                                                                
000070 01  GENERAL-SUBPROGRAMS.                                                 
000071*                                                                         
000072     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000073     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000074     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000075     SKIP2                                                                
000076*    --- PARAMETERS TO ABEND                                              
000077                                                                          
000078 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000079 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000080 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000081     SKIP2                                                                
000082 01  ERROR-TEXT.                                                          
000083     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
000084     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
000085     EJECT                                                                
000086*    --- PARAMETRAR TILL POSTSUM                                          
000087*                                                                         
000088*01  -COPY W0005   -PRE  POSTSUM-                                         
000089     EJECT                                                                
000095 01  IN1-AREA-START               PIC X(24)   VALUE                       
000096                                 'IN1-AREA-START  '.                      
000097                                                                          
000098*01  AREA -COPY W01183      -PRE IN1-                                     
000099     EJECT                                                                
000105 01  OUT2-AREA-START             PIC X(24)   VALUE                        
000106                                 'OUT2-AREA-START  '.                     
000107                                                                          
000108*01  AREA -COPY W01183X     -PRE OUTX1-                                   
000109     EJECT                                                                
000110 PROCEDURE DIVISION.                                                      
000111 MAIN SECTION.                                                            
000112     SKIP2                                                                
000113                                                                          
000114     PERFORM A-INIT                                                       
000115                                                                          
000123*CREATES WDK723 FILE IN READBLE FORMAT                                    
000124     PERFORM S01-READ-W01183                                              
000125     PERFORM UNTIL END-OF-W01183                                          
000126       PERFORM S11-WRITE-W01183X                                          
000127                                                                          
000128       PERFORM S01-READ-W01183                                            
000129     END-PERFORM                                                          
000130*                                                                         
000131                                                                          
000132     PERFORM Z-FINIT                                                      
000133                                                                          
000134     MOVE ZERO TO RETURN-CODE                                             
000135     GOBACK                                                               
000136     .                                                                    
000137     EJECT                                                                
000138 A-INIT SECTION.                                                          
000139                                                                          
000140     OPEN INPUT  W01183                                                   
000142          OUTPUT W01183X                                                  
000144     .                                                                    
000145     EJECT                                                                
000146 Z-FINIT SECTION.                                                         
000147     CLOSE W01183                                                         
000148           W01183X                                                        
000150                                                                          
000151     SKIP2                                                                
000152     MOVE 'S' TO POSTSUM-OPKOD                                            
000153     CALL POSTSUM USING POSTSUM-PARM                                      
000154     .                                                                    
000155     EJECT                                                                
000171 S01-READ-W01183   SECTION.                                               
000172                                                                          
000173     READ W01183  INTO IN1-AREA                                           
000174     AT END                                                               
000175        MOVE HIGH-VALUE TO IN1-AREA                                       
000176        SET END-OF-W01183  TO TRUE                                        
000177                                                                          
000178     NOT AT END                                                           
000179        MOVE 'W01183'   TO POSTSUM-FDNAMN                                 
000180        MOVE 'W01183D1' TO POSTSUM-DDNAMN2                                
000181        MOVE SPACE      TO POSTSUM-TRANSTYP                               
000182        CALL POSTSUM USING POSTSUM-PARM                                   
000183     END-READ                                                             
000184     .                                                                    
000185     EJECT                                                                
000261 S11-WRITE-W01183X SECTION.                                               
000262                                                                          
000263*WDK701                                                                   
000264     MOVE IN1-SAVT-IDARTNR             TO OUTX1-SAVT-IDARTNR              
000265     MOVE IN1-SAVT-IDDC                TO OUTX1-SAVT-IDDC                 
000266*WDK723                                                                   
000267     MOVE IN1-SAVT-IDAVTAL             TO OUTX1-SAVT-IDAVTAL              
000268     MOVE IN1-SAVT-IDLEVNR-AVT         TO OUTX1-SAVT-IDLEVNR-AVT          
000269     MOVE IN1-SAVT-IDLEVNR-SHIP        TO OUTX1-SAVT-IDLEVNR-SHIP         
000270     MOVE IN1-SAVT-TIAVTAL             TO OUTX1-SAVT-TIAVTAL              
000271                                                                          
000272     PERFORM S21A-WRITE-W01183X                                           
000273     .                                                                    
000274     EJECT                                                                
000275 S21A-WRITE-W01183X SECTION.                                              
000276                                                                          
000277     WRITE OUTX1-RECORD FROM OUTX1-AREA                                   
000278                                                                          
000279     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
000280     MOVE 'W01183X'  TO POSTSUM-FDNAMN                                    
000281     MOVE 'W01183D2' TO POSTSUM-DDNAMN2                                   
000282     CALL POSTSUM USING POSTSUM-PARM                                      
000283     .                                                                    
000284     EJECT                                                                
000285 S99-ABEND SECTION.                                                       
000286                                                                          
000287     SKIP2                                                                
000288     MOVE 'S' TO POSTSUM-OPKOD                                            
000289     CALL POSTSUM USING POSTSUM-PARM                                      
000290     CALL ABEND USING RKOD-ABEND                                          
000300     .                                                                    
