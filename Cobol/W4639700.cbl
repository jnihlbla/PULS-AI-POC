000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W4639570.                                                
000301 AUTHOR.         KJELLSON GÖRAN.                                          
000401 DATE-WRITTEN.   14/02/27.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*        REDIGERA FIL FÖR EJ SKICKADE DDGS-ORDER                          
000901*                                                                         
001001*        PROGRAMMET LÄSER      WDF6 (DIREKTLEVERANSER)                    
001101*                                                                         
001201* UTFIL W4639M - UPPFÖLJNINGSFIL EJ SKICKADE DDGS-ORDER                   
001301*                                                                         
001501*                                                                         
001601*    ABENDKODER:                                                          
001701*        U0016 -  . . . .                                                 
001801*        U1000 -  . . . .                                                 
002001                                                                          
002101                                                                          
002201 ENVIRONMENT DIVISION.                                                    
002301 INPUT-OUTPUT SECTION.                                                    
002401                                                                          
002501 FILE-CONTROL.                                                            
002601                                                                          
002701*          --- DIREKTLEVERANSLARM                                         
002801     SELECT W4639M                     ASSIGN TO W46397D1.                
003001                                                                          
003101 DATA DIVISION.                                                           
003201 FILE SECTION.                                                            
003301                                                                          
003401 FD  W4639M                                                               
003501     RECORDING       F                                                    
003601     BLOCK CONTAINS  0.                                                   
003701*01  POST      -COPY W4639M -PRE  DDGS-  -L.                              
003801                                                                          
003901                                                                          
004601 WORKING-STORAGE SECTION.                                                 
004701                                                                          
004801 77  IDPGM                       PIC X(8)    VALUE 'W4639700'.            
004901 77  JA                          PIC X       VALUE 'J'.                   
005001 77  NEJ                         PIC X       VALUE 'N'.                   
005101 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005201 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005301                                                                          
006001                                                                          
007001 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007901 01  CURRENT-DATE.                                                        
008101     03  FILLER                  PIC 9(2)    VALUE 20.                    
008201     03  CURRENT-AAMMDD          PIC 9(6)    VALUE ZERO.                  
008301                                                                          
008901                                                                          
009001 01  DYNAMISKA-SUBPROGRAM.                                                
009101                                                                          
009201     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009301     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009401     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009501     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009701                                                                          
009801*    --- PARAMETRAR TILL ABEND                                            
009901                                                                          
010001 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010101 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010201 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010301                                                                          
010401 01  FELTEXT.                                                             
010501     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010601     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010701                                                                          
010801*    --- PARAMETRAR TILL POSTSUM                                          
010901*                                                                         
011001*01  -COPY W0005   -PRE  POSTSUM-                                         
011101                                                                          
011601                                                                          
011701 01  DDGS-ORDER-START            PIC X(24)   VALUE                        
011801                                 'DDGS-ORDER-START '.                     
011901                                                                          
012001*01  AREA -COPY W4639M     -PRE DDGS-                                     
012101                                                                          
012201                                                                          
012202*    VALID DDGS                                                           
012203*01    -COPY WWLEV06                                                      
012204                                                                          
012901*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013001                                                                          
013101 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013201                                                                          
013205                                                                          
013301*    --- STATUS-KOD FRÅN IMS                                              
013401 01  STATUS-WS                   PIC XX.                                  
013501     88  SEGMENT-FINNS                       VALUE '  '.                  
013601     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013701     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013801                                                                          
013901 01  GODK-STATUSKODER.                                                    
014001     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014101                                                                          
014401 01  SSA1                     PIC X(64).                                  
014601                                                                          
014701*    --- IMS FUNKTIONSKODER                                               
014801*01  -COPY W0003                                                          
014901                                                                          
015001*    ---  DLI INPUT-OUTPUT AREA                                           
015101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF601'.                      
015201 01  DLI-IO-WDF601.                                                       
015301*    03  -COPY WDF601                                                     
015401                                                                          
015501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF611'.                      
015601 01  DLI-IO-WDF611.                                                       
015701*    03  -COPY WDF611                                                     
015801                                                                          
015901                                                                          
016001 LINKAGE SECTION.                                                         
016101                                                                          
016201                                                                          
016301*01  -COPY W0008  -PRE WDF6-                                              
016401     05  FILLER                  PIC X.                                   
016501                                                                          
016601                                                                          
016701 PROCEDURE DIVISION  USING WDF6-PCB.                                      
016801 MAIN SECTION.                                                            
016901     ENTRY 'DLITCBL' USING WDF6-PCB.                                      
017001                                                                          
017101                                                                          
017201     PERFORM A-INIT                                                       
017301                                                                          
017401     PERFORM IMS-GN-WDF601                                                
017501     PERFORM UNTIL SEGMENT-SLUT                                           
017601        MOVE PUDH-IDLEVNR TO LEV06-IDLEVNR                                
017701        IF LEV06-DDGS                                                     
017801           PERFORM IMS-GNP-WDF611                                         
017901           PERFORM UNTIL SEGMENT-SAKNAS                                   
018001                                                                          
018301              PERFORM B-SKRIV-DDGS-ORDER                                  
018702              PERFORM IMS-GNP-WDF611                                      
018801                                                                          
018901           END-PERFORM                                                    
019001        END-IF                                                            
019102        PERFORM IMS-GN-WDF601                                             
019201     END-PERFORM                                                          
019301                                                                          
019401     PERFORM Z-FINIT                                                      
019501                                                                          
019601     MOVE ZERO TO RETURN-CODE                                             
019701     GOBACK                                                               
019801     .                                                                    
019901                                                                          
020001                                                                          
020101 A-INIT SECTION.                                                          
020201     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
020301                                                                          
020402     OPEN OUTPUT W4639M                                                   
020501                                                                          
020601     ACCEPT DAGENS-DATUM  FROM DATE                                       
020701     MOVE DAGENS-DATUM      TO CURRENT-AAMMDD                             
020801     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020901     .                                                                    
021001                                                                          
021101                                                                          
027701 B-SKRIV-DDGS-ORDER SECTION.                                              
027801     MOVE 'B-SKRIV-DDGS    ' TO CURRENT-SECTION                           
027901                                                                          
031101     MOVE PUDH-IDLEVNR       TO DDGS-IDLEVNR                              
031102     MOVE PUDH-DASKEPPN      TO DDGS-DASKEPPN                             
031201     MOVE PUDR-IDARTNR       TO DDGS-IDARTNR                              
031301     MOVE PUDH-DASNDDAT      TO DDGS-DASNDDAT                             
031401     MOVE PUDH-IDDISTR       TO DDGS-IDDISTR                              
031501     MOVE PUDH-IDKUNDNR      TO DDGS-IDKUNDNR                             
031601     MOVE PUDH-IDORDNR7      TO DDGS-IDORDNR7                             
031701     MOVE PUDH-IDPRODNR      TO DDGS-IDPRODNR                             
031801     MOVE PUDR-KVBEART       TO DDGS-KVBEART                              
031901     MOVE PUDH-KDORDKL       TO DDGS-KDORDKL                              
032001     MOVE PUDR-IDPURAD       TO DDGS-IDPURAD                              
032901                                                                          
033001     PERFORM S11-SKRIV-W4639M                                             
033101     .                                                                    
033201                                                                          
033301                                                                          
044001 Z-FINIT SECTION.                                                         
044101     CLOSE W4639M                                                         
044201                                                                          
044301     MOVE 'S'        TO POSTSUM-OPKOD                                     
044401     CALL POSTSUM USING POSTSUM-PARM                                      
044501     .                                                                    
044601                                                                          
044701                                                                          
044801 S11-SKRIV-W4639M SECTION.                                                
044901                                                                          
045001     WRITE DDGS-POST FROM DDGS-AREA                                       
045101                                                                          
045201     MOVE 'DDGS'      TO POSTSUM-TRANSTYP                                 
045301     MOVE 'W4639M'    TO POSTSUM-FDNAMN                                   
045401     MOVE 'W46397D1'  TO POSTSUM-DDNAMN2                                  
045501     CALL POSTSUM  USING POSTSUM-PARM                                     
045601     .                                                                    
045701                                                                          
045801                                                                          
047001                                                                          
047101* --- IMS SEKTIONER ---                                                   
047201                                                                          
047301                                                                          
047401 IMS-GN-WDF601 SECTION.                                                   
047502     MOVE 'IMS-GN-WDF601   ' TO CURRENT-SECTION                           
047601                                                                          
047701                                                                          
047801     MOVE 'WDF601 '           TO SSA1                                     
047901     STRING 'WDF601  (DASNDDAT >' CURRENT-DATE ')'                        
048001          DELIMITED BY SIZE INTO SSA1                                     
048101     MOVE '  GB'              TO GODK-STATUSKODER                         
048201     CALL CBLTDLI USING GN WDF6-PCB DLI-IO-WDF601 SSA1                    
048301     MOVE WDF6-STATUS-CODE    TO STATUS-WS                                
048401     PERFORM IMS-STATUSKONTROLL                                           
048501     .                                                                    
048601                                                                          
048701 IMS-GNP-WDF611 SECTION.                                                  
048801     MOVE 'IMS-GNP-WDF611  ' TO CURRENT-SECTION                           
048901                                                                          
049001                                                                          
049101     MOVE 'WDF611 '           TO SSA1                                     
049201     MOVE '  GE'              TO GODK-STATUSKODER                         
049301     CALL CBLTDLI USING GNP WDF6-PCB DLI-IO-WDF611 SSA1                   
049401     MOVE WDF6-STATUS-CODE    TO STATUS-WS                                
049501     PERFORM IMS-STATUSKONTROLL                                           
049601     .                                                                    
049701                                                                          
049801 IMS-STATUSKONTROLL SECTION.                                              
049901                                                                          
050001     SET STATUS-IX TO 1                                                   
050101     SEARCH GODK-STATUS                                                   
050201       AT END                                                             
050301         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
050401           DELIMITED BY SIZE INTO FELTEXT                                 
050501         DISPLAY FELTEXT                                                  
050601         CALL FELLOG                                                      
050701       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
050801         CONTINUE                                                         
051001     END-SEARCH                                                           
060001     .                                                                    
