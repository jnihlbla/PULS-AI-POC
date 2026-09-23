000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W3352300.                                                
000003 AUTHOR.         ELEONOR ÖSTRÖM.                                          
000004 DATE-WRITTEN.   07/10/17.                                                
000005                                                                          
000006*    FUNKTION:                                                            
000007*                                                                         
000008*                                                                         
000009*        PROGRAMMET LÄSER      WDB2                                       
000010*                                                                         
000011*    ABENDKODER:                                                          
000012*        U0016 -  . . . .                                                 
000013*        U1000 -  . . . .                                                 
000014*                                                                         
000015                                                                          
000016     SKIP3                                                                
000017 ENVIRONMENT DIVISION.                                                    
000018     SKIP2                                                                
000019 INPUT-OUTPUT SECTION.                                                    
000020                                                                          
000021 FILE-CONTROL.                                                            
000022     SKIP2                                                                
000023*          --- FIL FRÅN DATABASEN WDB2                                    
000024     SELECT W33523                     ASSIGN TO W33523D1.                
000025     EJECT                                                                
000026 DATA DIVISION.                                                           
000027     SKIP2                                                                
000028 FILE SECTION.                                                            
000029     SKIP3                                                                
000030 FD  W33523                                                               
000031     RECORDING       F                                                    
000032     BLOCK CONTAINS  0.                                                   
000033                                                                          
000034*01  POST -COPY W33523 -PRE  UT-  -L.                                     
000035     EJECT                                                                
000036 WORKING-STORAGE SECTION.                                                 
000037                                                                          
000038                                                                          
000039*    -- CHECKED BY WY2000                                                 
000040 77  IDPGM                       PIC X(8)    VALUE 'W3352300'.            
000041 77  JA                          PIC X       VALUE 'J'.                   
000042 77  NEJ                         PIC X       VALUE 'N'.                   
000043     EJECT                                                                
000044 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000045 01  FILLER REDEFINES DAGENS-DATUM.                                       
000046     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000047     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000048     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000049     EJECT                                                                
000050 01  DYNAMISKA-SUBPROGRAM.                                                
000051*                                                                         
000052     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000053     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000054     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000055     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000056     SKIP2                                                                
000057*    --- PARAMETRAR TILL ABEND                                            
000058                                                                          
000059 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000060 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000061     SKIP2                                                                
000062 01  FELTEXT.                                                             
000063     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000064     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000065     EJECT                                                                
000066*    --- PARAMETRAR TILL POSTSUM                                          
000067*                                                                         
000068*01  -COPY W0005   -PRE  POSTSUM-                                         
000069     EJECT                                                                
000070 01  UT-AREA-START               PIC X(24)   VALUE                        
000071                                 'UT-AREA-START  '.                       
000072     SKIP2                                                                
000073                                                                          
000074*01  AREA -COPY W33523     -PRE UT-                                       
000075     EJECT                                                                
000076*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000077*                                                                         
000078     EJECT                                                                
000079 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000080     SKIP3                                                                
000081*    --- STATUS-KOD FRÅN IMS                                              
000082 01  STATUS-WS                   PIC XX.                                  
000083     88  SEGMENT-FINNS                       VALUE '  '.                  
000084     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000085     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000086     SKIP2                                                                
000087 01  GODK-STATUSKODER.                                                    
000088     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000089     SKIP3                                                                
000090 01  SSA1                        PIC X(64).                               
000091 01  SSA2                        PIC X(64).                               
000092     EJECT                                                                
000093*    --- IMS FUNKTIONSKODER                                               
000094*01  -COPY W0003                                                          
000095     EJECT                                                                
000096*    ---  DLI INPUT-OUTPUT AREA                                           
000097 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000098                                                                          
000099 01  DLI-IO-AREA.                                                         
000100*  03  -COPY WDB201  -PRE WDB2-                                           
000101     EJECT                                                                
000102 LINKAGE SECTION.                                                         
000103*01  -COPY W0008  -PRE WDB2-                                              
000104     05  FILLER                  PIC X.                                   
000105     EJECT                                                                
000106 PROCEDURE DIVISION  USING WDB2-PCB.                                      
000107 MAIN SECTION.                                                            
000108     ENTRY 'DLITCBL' USING WDB2-PCB.                                      
000109                                                                          
000110     PERFORM A-INIT                                                       
000111     PERFORM IMS-GET-WDB2                                                 
000112     PERFORM UNTIL SEGMENT-SLUT                                           
000113       PERFORM B-FLYTTA-DISTRIKT                                          
000114       PERFORM IMS-GET-WDB2                                               
000115     END-PERFORM                                                          
000116                                                                          
000117                                                                          
000118     PERFORM Z-FINIT                                                      
000119                                                                          
000120     MOVE ZERO TO RETURN-CODE                                             
000121     GOBACK                                                               
000122     .                                                                    
000123     EJECT                                                                
000124 A-INIT SECTION.                                                          
000125                                                                          
000126     OPEN OUTPUT W33523                                                   
000127                                                                          
000128     INITIALIZE UT-W33523                                                 
000129     ACCEPT DAGENS-DATUM  FROM DATE                                       
000130     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000131     .                                                                    
000132     EJECT                                                                
000133 B-FLYTTA-DISTRIKT SECTION.                                               
000134                                                                          
000135     MOVE WDB2-GMT-IDPARTNR     TO UT-IDPARTNR                            
000136     MOVE WDB2-GMT-IDDISTR      TO UT-IDDISTR                             
000137     PERFORM S11-SKRIV-W33523                                             
000138     .                                                                    
000139     EJECT                                                                
000140 Z-FINIT SECTION.                                                         
000141                                                                          
000142     CLOSE W33523                                                         
000143                                                                          
000144     MOVE 'S' TO POSTSUM-OPKOD                                            
000145     CALL POSTSUM USING POSTSUM-PARM                                      
000146     .                                                                    
000147     EJECT                                                                
000148 S11-SKRIV-W33523 SECTION.                                                
000149                                                                          
000150     WRITE UT-POST FROM UT-AREA                                           
000151     MOVE 'W335'    TO POSTSUM-TRANSTYP                                   
000152     MOVE 'W33523' TO POSTSUM-FDNAMN                                      
000153     MOVE 'W33523D1' TO POSTSUM-DDNAMN2                                   
000154     CALL POSTSUM USING POSTSUM-PARM                                      
000155     .                                                                    
000156* --- IMS SEKTIONER ---                                                   
000157     EJECT                                                                
000158 IMS-GET-WDB2   SECTION.                                                  
000159                                                                          
000160     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-AREA                           
000161     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
000162     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000163     PERFORM IMS-STATUSKONTROLL                                           
000164     .                                                                    
000165     EJECT                                                                
000166 IMS-STATUSKONTROLL SECTION.                                              
000167                                                                          
000168     SET STATUS-IX TO 1                                                   
000169     SEARCH GODK-STATUS                                                   
000170       AT END                                                             
000171         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
000172         DISPLAY FELTEXT                                                  
000173         CALL FELLOG                                                      
000174       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000175         CONTINUE                                                         
000176     END-SEARCH                                                           
000177     .                                                                    
