000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W3357300.                                                
000004 AUTHOR.         INGVAR SKJELBRED.                                        
000005 DATE-WRITTEN.   96/06/13.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        LÄSER NER KUNDREGISTRET                                          
000010*                                                                         
000011*                                                                         
000012*                                                                         
000013*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
000014*        PROGRAMMET LÄSER      WDB101                                     
000015*                                                                         
000016*    ABENDKODER:                                                          
000017*        U0016 -  . . . .                                                 
000018*        U1000 -  . . . .                                                 
000019*                                                                         
000020                                                                          
000021     SKIP3                                                                
000022 ENVIRONMENT DIVISION.                                                    
000023     SKIP2                                                                
000024 INPUT-OUTPUT SECTION.                                                    
000025                                                                          
000026 FILE-CONTROL.                                                            
000027     SKIP2                                                                
000028*          --- UT FIL                                                     
000029     SELECT W33573                     ASSIGN TO W33573D1.                
000030     EJECT                                                                
000031 DATA DIVISION.                                                           
000032     SKIP2                                                                
000033 FILE SECTION.                                                            
000034     SKIP3                                                                
000035 FD  W33573                                                               
000036     RECORDING       F                                                    
000037     BLOCK CONTAINS  0.                                                   
000038                                                                          
000039*01  POST -COPY W33573 -PRE  UT-  -L.                                     
000040     EJECT                                                                
000041 WORKING-STORAGE SECTION.                                                 
000042                                                                          
000043                                                                          
000044*    -- CHECKED BY WY2000                                                 
000045 77  IDPGM                       PIC X(8)    VALUE 'W3357300'.            
000046 77  JA                          PIC X       VALUE 'J'.                   
000047 77  NEJ                         PIC X       VALUE 'N'.                   
000048 77  SPAR-IDDISTR PIC 9(4) VALUE ZERO.                                    
000049 77  SPAR2-IDDISTR PIC 9(4) VALUE ZERO.                                   
000050     EJECT                                                                
000051 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000052 01  FILLER REDEFINES DAGENS-DATUM.                                       
000053     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000054     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000055     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000056     EJECT                                                                
000057 01  DYNAMISKA-SUBPROGRAM.                                                
000058*                                                                         
000059     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000060     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000061     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000062     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000063     SKIP2                                                                
000064*    --- PARAMETRAR TILL ABEND                                            
000065                                                                          
000066 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000067 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000068     SKIP2                                                                
000069 01  FELTEXT.                                                             
000070     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000071     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000072     EJECT                                                                
000073*    --- PARAMETRAR TILL POSTSUM                                          
000074*                                                                         
000075*01  -COPY W0005   -PRE  POSTSUM-                                         
000076     EJECT                                                                
000077 01  UT-AREA-START               PIC X(24)   VALUE                        
000078                                 'UT-AREA-START  '.                       
000079     SKIP2                                                                
000080                                                                          
000081*01  AREA -COPY W33573     -PRE UT-                                       
000082     EJECT                                                                
000083*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000084*                                                                         
000085     EJECT                                                                
000086 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000087     SKIP3                                                                
000088 01  NYCKLAR-TILL-DLI.                                                    
000089     03  W-IDGMT-X.                                                       
000090         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000091         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000092     03  W-WDB101KY-X.                                                    
000093         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
000094         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
000095     SKIP2                                                                
000096*    --- STATUS-KOD FRÅN IMS                                              
000097 01  STATUS-WS                   PIC XX.                                  
000098     88  SEGMENT-FINNS                       VALUE '  '.                  
000099     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000101     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000102     SKIP2                                                                
000103 01  GODK-STATUSKODER.                                                    
000104     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000105     SKIP3                                                                
000106 01  SSA1                        PIC X(64).                               
000107 01  SSA2                        PIC X(64).                               
000108     EJECT                                                                
000109*    --- IMS FUNKTIONSKODER                                               
000110*01  -COPY W0003                                                          
000111     EJECT                                                                
000112*    ---  DLI INPUT-OUTPUT AREA                                           
000113 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000114                                                                          
000115 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
000116 01  DLI-IO-B201.                                                         
000117*    03  -COPY WDB201 -PRE GMTA-                                          
000118                                                                          
000119 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB101'.           
000120 01  DLI-IO-B101.                                                         
000121*    03  -COPY WDB101 -PRE WDB1-                                          
000122                                                                          
000131     EJECT                                                                
000132 LINKAGE SECTION.                                                         
000133                                                                          
000134     EJECT                                                                
000135*01  -COPY W0008  -PRE GMTA-                                              
000136     05  FILLER                  PIC X.                                   
000137     EJECT                                                                
000138*01  -COPY W0008  -PRE WDB1-                                              
000139     05  FILLER                  PIC X.                                   
000140     EJECT                                                                
000141 PROCEDURE DIVISION  USING GMTA-PCB WDB1-PCB.                             
000142 MAIN SECTION.                                                            
000143     ENTRY 'DLITCBL' USING GMTA-PCB WDB1-PCB.                             
000144                                                                          
000145     PERFORM A-INIT                                                       
000146     PERFORM IMS-GET-GMTA-WLGMTA01                                        
000147     PERFORM UNTIL SEGMENT-SAKNAS                                         
000148                 OR  SEGMENT-SLUT                                         
000149        PERFORM B-FLYTTA-DISTRIKT                                         
000150        IF SPAR-IDDISTR NOT = SPAR2-IDDISTR                               
000151          PERFORM S11-SKRIV-W33573                                        
000152          MOVE SPAR2-IDDISTR         TO SPAR-IDDISTR                      
000153        END-IF                                                            
000154        PERFORM IMS-GET-GMTA-WLGMTA01                                     
000155     END-PERFORM                                                          
000156                                                                          
000157     PERFORM Z-FINIT                                                      
000158                                                                          
000159     MOVE ZERO TO RETURN-CODE                                             
000160     GOBACK                                                               
000161     .                                                                    
000162     EJECT                                                                
000163 A-INIT SECTION.                                                          
000164                                                                          
000165     OPEN OUTPUT W33573                                                   
000166                                                                          
000167     ACCEPT DAGENS-DATUM  FROM DATE                                       
000168     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000169     .                                                                    
000170     EJECT                                                                
000171 B-FLYTTA-DISTRIKT SECTION.                                               
000172                                                                          
000173     MOVE GMTA-GMT-IDDISTR         TO SPAR2-IDDISTR                       
000174                                      UT-IDDISTR                          
000175     MOVE GMTA-GMT-REEMBHNT        TO UT-REEMBHNT                         
000176     MOVE GMTA-GMT-IDPARTNR        TO W-WDB1-IDPARTNR                     
000177                                      UT-IDPARTNR                         
000178     MOVE GMTA-GMT-IDFTG           TO W-WDB1-IDFTG                        
000179     MOVE GMTA-GMT-FLOKFAK-G       TO UT-FLOKFAK-G                        
000180     MOVE GMTA-GMT-FLOKFAK-K       TO UT-FLOKFAK-K                        
000181     MOVE GMTA-GMT-FLOKFAK-N       TO UT-FLOKFAK-N                        
000182     MOVE GMTA-GMT-FLOKFAK-R       TO UT-FLOKFAK-R                        
000183     PERFORM IMS-GET-WDB101                                               
000184     IF SEGMENT-FINNS                                                     
000185        MOVE WDB1-BET-KDVALISO        TO UT-KDVALISO                      
000186        MOVE WDB1-BET-BEBETRAD-1      TO UT-BET-BEBETRAD-1                
000187     ELSE                                                                 
000188        MOVE SPACE                    TO UT-KDVALISO                      
000189        MOVE 'MISSING'                TO UT-BET-BEBETRAD-1                
000190     END-IF                                                               
000191     .                                                                    
000192     EJECT                                                                
000193 Z-FINIT SECTION.                                                         
000194     CLOSE W33573                                                         
000195     SKIP2                                                                
000196     MOVE 'S' TO POSTSUM-OPKOD                                            
000197     CALL POSTSUM USING POSTSUM-PARM                                      
000198     .                                                                    
000199     EJECT                                                                
000200 S11-SKRIV-W33573 SECTION.                                                
000201                                                                          
000202     WRITE UT-POST FROM UT-AREA                                           
000203                                                                          
000204     MOVE 'W33573' TO POSTSUM-FDNAMN                                      
000205     MOVE 'W33573D1' TO POSTSUM-DDNAMN2                                   
000206     CALL POSTSUM USING POSTSUM-PARM                                      
000207     .                                                                    
000208                                                                          
000209* --- IMS SEKTIONER ---                                                   
000210                                                                          
000211     EJECT                                                                
000212 IMS-GET-GMTA-WLGMTA01 SECTION.                                           
000213                                                                          
000214     MOVE 'WLGMTA01' TO SSA1                                              
000215     MOVE '  GBGE' TO GODK-STATUSKODER                                    
000216     CALL CBLTDLI USING GN GMTA-PCB DLI-IO-B201 SSA1                      
000217     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
000218     PERFORM IMS-STATUSKONTROLL                                           
000219     .                                                                    
000220     EJECT                                                                
000221 IMS-GET-WDB101 SECTION.                                                  
000222                                                                          
000223     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
000224          DELIMITED BY SIZE INTO SSA1                                     
000225     MOVE '  GE' TO GODK-STATUSKODER                                      
000226     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-B101 SSA1                      
000227     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000228     PERFORM IMS-STATUSKONTROLL                                           
000229     .                                                                    
000230     EJECT                                                                
000231 IMS-STATUSKONTROLL SECTION.                                              
000232                                                                          
000233     SET STATUS-IX TO 1                                                   
000234     SEARCH GODK-STATUS                                                   
000235       AT END                                                             
000236         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000237           DELIMITED BY SIZE INTO FELTEXT                                 
000238         DISPLAY FELTEXT                                                  
000239         CALL FELLOG                                                      
000240       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000241         CONTINUE                                                         
000242     END-SEARCH                                                           
000250     .                                                                    
