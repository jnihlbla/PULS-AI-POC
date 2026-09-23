001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4790900.                                                
001200 AUTHOR.         LASSI OLGRENER.                                          
001300 DATE-WRITTEN.   00/03/03.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        LÄSER NER WDE4 TILL FIL W47909 MED 01 OCH 11-SEGMENT INFO        
002000*                                                                         
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- WDE401/11 POSTER                                           
003210     SELECT W47909                     ASSIGN TO W47909D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W47909                                                               
003803     RECORDING       V                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003806*01  POST -COPY W479E401 -PRE  E401-  -L.                                 
003807                                                                          
003810*01  POST -COPY W479E411 -PRE  E411-  -L.                                 
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W4790900'.            
004700     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  UT-AREA-START               PIC X(24)   VALUE                        
007203                                 'UT-AREA-START  '.                       
007204     SKIP2                                                                
007210*01  AREA -COPY W479E401  -PRE E401-                                      
007220*01  AREA -COPY W479E411  -PRE E411-                                      
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008600     SKIP2                                                                
008700 01  GODK-STATUSKODER.                                                    
008800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     EJECT                                                                
009300*    --- IMS FUNKTIONSKODER                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE4'.                        
009802 01  DLI-IO-WDE4.                                                         
009803     03  IO-WDE4    PIC X(500).                                           
009810*    03  -COPY WDE401  -RED IO-WDE4.                                      
009820*    03  -COPY WDE411  -RED IO-WDE4.                                      
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010401                                                                          
010402*01  -COPY W0008  -PRE WDE4-                                              
010410     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010601 PROCEDURE DIVISION  USING WDE4-PCB.                                      
010602 MAIN SECTION.                                                            
010610     ENTRY 'DLITCBL' USING WDE4-PCB.                                      
010900                                                                          
011000     PERFORM A-INIT                                                       
011100                                                                          
011201     PERFORM IMS-GET-WDE4                                                 
011202     PERFORM UNTIL SEGMENT-SLUT                                           
011203       EVALUATE WDE4-SEG-NAME-FB                                          
011205         WHEN 'WDE401'                                                    
011206           PERFORM B-SKRIV-E401-POSTER                                    
011207         WHEN 'WDE411'                                                    
011208           PERFORM C-SKRIV-E411-POSTER                                    
011209       END-EVALUATE                                                       
011210       PERFORM IMS-GET-WDE4                                               
011220     END-PERFORM                                                          
011300     PERFORM Z-FINIT                                                      
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012101                                                                          
012110     OPEN OUTPUT W47909                                                   
012200                                                                          
012410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     .                                                                    
012700     EJECT                                                                
012710 B-SKRIV-E401-POSTER SECTION.                                             
012711                                                                          
012712     MOVE 'WDE401'         TO E401-IDSEGM                                 
012713     MOVE KORD-IDGMTREF    TO E401-IDGMTREF                               
012714     MOVE KORD-IDPRODNR    TO E401-IDPRODNR                               
012715     MOVE KORD-IDPLKLST    TO E401-IDPLKLST                               
012716     MOVE KORD-IDORDER     TO E401-IDORDER                                
012717     MOVE KORD-IDDC        TO E401-IDDC                                   
012718                                                                          
012719     PERFORM S11-SKRIV-W47909-E401                                        
012720     .                                                                    
012730     EJECT                                                                
012740 C-SKRIV-E411-POSTER SECTION.                                             
012750                                                                          
012751     MOVE 'WDE411'         TO E411-IDSEGM                                 
012752     MOVE E401-IDGMTREF    TO E411-IDGMTREF                               
012753     MOVE E401-IDPRODNR    TO E411-IDPRODNR                               
012754     MOVE ORAD-IDARTNR     TO E411-IDARTNR                                
012755     MOVE ORAD-IDKUNDRF-RO TO E411-IDKUNDRF-RO                            
012756     MOVE ORAD-IDLOPNR-RO  TO E411-IDLOPNR-RO                             
012757                                                                          
012758     PERFORM S12-SKRIV-W47909-E411                                        
012760     .                                                                    
012770     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012900                                                                          
012910     CLOSE W47909                                                         
013001     SKIP2                                                                
013002     MOVE 'S' TO POSTSUM-OPKOD                                            
013010     CALL POSTSUM USING POSTSUM-PARM                                      
013100     .                                                                    
013301     EJECT                                                                
013302 S11-SKRIV-W47909-E401 SECTION.                                           
013303                                                                          
013304     WRITE E401-POST FROM E401-AREA                                       
013305                                                                          
013306     MOVE 'E401'     TO POSTSUM-TRANSTYP                                  
013307     MOVE 'W47909'   TO POSTSUM-FDNAMN                                    
013308     MOVE 'W47909D1' TO POSTSUM-DDNAMN2                                   
013309     CALL POSTSUM USING POSTSUM-PARM                                      
013310     .                                                                    
013500     EJECT                                                                
013510 S12-SKRIV-W47909-E411 SECTION.                                           
013520                                                                          
013530     WRITE E411-POST FROM E411-AREA                                       
013540                                                                          
013550     MOVE 'E411'     TO POSTSUM-TRANSTYP                                  
013560     MOVE 'W47909'   TO POSTSUM-FDNAMN                                    
013570     MOVE 'W47909D1' TO POSTSUM-DDNAMN2                                   
013580     CALL POSTSUM USING POSTSUM-PARM                                      
013590     .                                                                    
013591     EJECT                                                                
014200* --- IMS SEKTIONER ---                                                   
014401                                                                          
014402 IMS-GET-WDE4   SECTION.                                                  
014403                                                                          
014404     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-WDE4                           
014405     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
014406     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014407     PERFORM IMS-STATUSKONTROLL                                           
014410     .                                                                    
014500     EJECT                                                                
014600 IMS-STATUSKONTROLL SECTION.                                              
014700                                                                          
014800     SET STATUS-IX TO 1                                                   
014900     SEARCH GODK-STATUS                                                   
015000       AT END                                                             
015100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015200           DELIMITED BY SIZE INTO FELTEXT                                 
015300         DISPLAY FELTEXT                                                  
015400         CALL FELLOG                                                      
015500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015600         CONTINUE                                                         
015700     END-SEARCH                                                           
015800     .                                                                    
