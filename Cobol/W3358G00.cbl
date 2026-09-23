000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W3358G00.                                                
000003 AUTHOR.         ANDERS HENRIKSSON                                        
000004 DATE-WRITTEN.   2012-02-20                                               
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        LÄSER NER WDB6                                                   
000009*                                                                         
000010*        PROGRAMMET LÄSER      WDB6                                       
000011*                                                                         
000012                                                                          
000013 ENVIRONMENT DIVISION.                                                    
000014 INPUT-OUTPUT SECTION.                                                    
000015 FILE-CONTROL.                                                            
000016     SKIP2                                                                
000017*          --- FIL MED PROCENTSATSER KALKYLPÅLÄGG                         
000018     SELECT W3358G                     ASSIGN TO W3358GD1.                
000019     SKIP2                                                                
000020 DATA DIVISION.                                                           
000030     SKIP3                                                                
000031 FILE SECTION.                                                            
000032     SKIP3                                                                
000033 FD  W3358G                                                               
000034     RECORDING       F                                                    
000035     BLOCK CONTAINS  0.                                                   
000036     SKIP2                                                                
000037*01  POST -COPY W3358G -PRE  UT-  -L.                                     
000038     SKIP3                                                                
000039 WORKING-STORAGE SECTION.                                                 
000040                                                                          
000050 77  IDPGM                       PIC X(8)    VALUE 'W3358G00'.            
000060 77  JA                          PIC X       VALUE 'J'.                   
000061 77  NEJ                         PIC X       VALUE 'N'.                   
000062                                                                          
000063 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000064 01  FILLER REDEFINES DAGENS-DATUM.                                       
000065     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000066     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000067     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000068     EJECT                                                                
000069                                                                          
000070 01  DYNAMISKA-SUBPROGRAM.                                                
000071     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000072     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000073     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000074     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000075                                                                          
000076*    --- PARAMETRAR TILL ABEND                                            
000077 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000078 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000079                                                                          
000080 01  FELTEXT.                                                             
000090     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000091     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000092                                                                          
000093*    --- PARAMETRAR TILL POSTSUM                                          
000094*01  -COPY W0005   -PRE  POSTSUM-                                         
000095                                                                          
000096 01  UT-AREA-START           PIC X(24)   VALUE                            
000097                                 'UT-AREA-START'.                         
000098*01  AREA -COPY W3358G     -PRE UT-                                       
000099                                                                          
000100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000110 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000120                                                                          
000121*    --- STATUS-KOD FRÅN IMS                                              
000122 01  STATUS-WS                   PIC XX.                                  
000123     88  SEGMENT-FINNS                       VALUE '  '.                  
000124     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000125                                                                          
000126 01  GODK-STATUSKODER.                                                    
000127     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000128                                                                          
000129 01  SSA1                        PIC X(128).                              
000130 01  SSA2                        PIC X(128).                              
000131                                                                          
000132*    --- IMS FUNKTIONSKODER                                               
000133*01  -COPY W0003                                                          
000134                                                                          
000135*    ---  DLI INPUT-OUTPUT AREA                                           
000136 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000137 01  DLI-IO-AREA.                                                         
000138     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
000139     EJECT                                                                
000140     03  WDB601 REDEFINES IO-AREA.                                        
000141*        05  -COPY WDB601                                                 
000142     EJECT                                                                
000143     03  WDB617 REDEFINES IO-AREA.                                        
000144*        05  -COPY WDB617                                                 
000145     EJECT                                                                
000146                                                                          
000147 LINKAGE SECTION.                                                         
000148*01  -COPY W0008  -PRE WDB6-                                              
000149     05  FILLER                  PIC X.                                   
000150     EJECT                                                                
000151 PROCEDURE DIVISION  USING WDB6-PCB.                                      
000152     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
000153                                                                          
000154     SKIP2                                                                
000155     PERFORM A-INIT                                                       
000156                                                                          
000157     PERFORM B-INPUT                                                      
000158                                                                          
000159     PERFORM Z-FINIT                                                      
000160                                                                          
000170     MOVE ZERO TO RETURN-CODE                                             
000171     GOBACK                                                               
000172     .                                                                    
000173     EJECT                                                                
000174                                                                          
000175 A-INIT SECTION.                                                          
000176     SKIP2                                                                
000177     ACCEPT DAGENS-DATUM  FROM DATE                                       
000178     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000179     OPEN OUTPUT W3358G                                                   
000180     .                                                                    
000181     EJECT                                                                
000182                                                                          
000183 B-INPUT SECTION.                                                         
000184     PERFORM IMS-GET-WDB6                                                 
000185     PERFORM UNTIL SEGMENT-SLUT                                           
000186       EVALUATE WDB6-SEG-NAME-FB                                          
000187         WHEN 'WDB601  '                                                  
000188           MOVE DCS-IDDC           TO UT-IDDC                             
000189         WHEN 'WDB617  '                                                  
000190           MOVE PROC-REDIRLON      TO UT-REDIRLON                         
000200           MOVE PROC-REDMTRL       TO UT-REDMTRL                          
000210           PERFORM S31-SKRIV-W3358G                                       
000220       END-EVALUATE                                                       
000230       PERFORM IMS-GET-WDB6                                               
000240     END-PERFORM                                                          
000250*    PERFORM S31-SKRIV-W3358G                                             
000260     .                                                                    
000261     EJECT                                                                
000262                                                                          
000263 Z-FINIT SECTION.                                                         
000264     CLOSE W3358G                                                         
000265     MOVE 'S' TO POSTSUM-OPKOD                                            
000266     CALL POSTSUM USING POSTSUM-PARM                                      
000267     .                                                                    
000268     EJECT                                                                
000269                                                                          
000270 S31-SKRIV-W3358G SECTION.                                                
000271     WRITE UT-POST FROM UT-AREA                                           
000272                                                                          
000273     MOVE 'WDB6'  TO POSTSUM-TRANSTYP                                     
000274     MOVE 'W3358G' TO POSTSUM-FDNAMN                                      
000275     MOVE 'W3358GD1' TO POSTSUM-DDNAMN2                                   
000276     CALL POSTSUM USING POSTSUM-PARM                                      
000277     .                                                                    
000278     EJECT                                                                
000279                                                                          
000280 S99-ABEND SECTION.                                                       
000290     MOVE 'S' TO POSTSUM-OPKOD                                            
000291     CALL POSTSUM USING POSTSUM-PARM                                      
000292     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
000293     .                                                                    
000294     EJECT                                                                
000295                                                                          
000296* --- IMS SEKTIONER ---                                                   
000297 IMS-GET-WDB6   SECTION.                                                  
000298     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA                           
000299     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
000300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000301     PERFORM IMS-STATUSKONTROLL                                           
000302     .                                                                    
000303     EJECT                                                                
000304                                                                          
000305 IMS-STATUSKONTROLL SECTION.                                              
000306     SET STATUS-IX TO 1                                                   
000307     SEARCH GODK-STATUS                                                   
000308       AT END                                                             
000309         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
000310         DISPLAY FELTEXT                                                  
000311         CALL FELLOG                                                      
000312       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000313         CONTINUE                                                         
000314     END-SEARCH                                                           
000315     .                                                                    
