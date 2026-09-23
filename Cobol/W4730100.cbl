000101 ID DIVISION.                                                             
000202 PROGRAM-ID.           W4730100.                                          
000301 AUTHOR.               LASSI.                                             
000401 DATE-WRITTEN.         JAN 2025.                                          
000501                                                                          
000601*    REMARKS.                                                             
000701*                                                                         
000801*    FUNKTION:         LÄSER WDG2 HTYP 4321                               
001001*                      LÄSER WDE4.                                        
001101*                      LÄSER WDC7,FÖR RADER MED PRLIMINÄRT PRIS.          
001201*                                                                         
001301*                      POSTER MED INFORMATION PACKNING KOLLI              
001401*                      SKRIVS TILL SVENSKA ÅF.                            
001901     EJECT                                                                
002001 ENVIRONMENT DIVISION.                                                    
002101                                                                          
002201 INPUT-OUTPUT SECTION.                                                    
002301 FILE-CONTROL.                                                            
002401* PACKED CASE TO VR                                                       
002502     SELECT  W47301   ASSIGN TO W47301D1.                                 
002601                                                                          
002701* CLEANUP RECORDS WDG202 > WDGX4322                                       
002802     SELECT  W47303   ASSIGN TO W47301D2.                                 
002901                                                                          
003001 DATA DIVISION.                                                           
003101 FILE SECTION.                                                            
003201                                                                          
003301 FD  W47301                                                               
003401     LABEL RECORD STANDARD                                                
003501     RECORDING      V                                                     
003601     BLOCK CONTAINS 0.                                                    
003701                                                                          
003801*01  W47301-POST -COPY W4730111     -L                                    
003901                                                                          
004002 FD  W47303                                                               
004101     LABEL RECORD STANDARD                                                
004201     RECORDING      F                                                     
004301     BLOCK CONTAINS 0.                                                    
004401                                                                          
004502*01  W47303-POST -COPY W47303       -L                                    
004601     EJECT                                                                
004701 WORKING-STORAGE SECTION.                                                 
004801                                                                          
004901*    -- CHECKED BY WY2000                                                 
005001*                                                                         
005101*  ------------------------------ GENERELLA KONSTANTER                    
005201 77  JA                           PIC X     VALUE 'J'.                    
005301 77  NEJ                          PIC X     VALUE 'N'.                    
005401*                                                                         
005501*  ------------------------------ ARBETSVARIABLER                         
005601*                                                                         
005701 77  PGMPOS                       PIC X(24) VALUE SPACE.                  
006001                                                                          
006101     EJECT                                                                
006201*  ---------------------------- SUBPROGRAM OCH PARAMETERAREOR             
006301                                                                          
006401 01  DYNAMISKA-SUBPROGRAM.                                                
006501     03  POSTSUM                  PIC X(8)  VALUE 'POSTSUM '.             
006601     03  CBLTDLI                  PIC X(8)  VALUE 'CBLTDLI '.             
006701     03  FELLOG                   PIC X(8)  VALUE 'FELLOG  '.             
006801     EJECT                                                                
006901*  ---------------------------- PARAMETRAR TILL POSTSUM                   
007001                                                                          
007101*01 -COPY W0005       -PRE POSTSUM-                                       
007201     EJECT                                                                
007301 01  FILLER              PIC X(16)  VALUE 'UTSKRIFTSPOSTER'.              
007401*                                                                         
007501*01 -COPY W4730100    -PRE 100-                                           
007601     SKIP3                                                                
007701*01 -COPY W4730111    -PRE 111-                                           
007801     EJECT                                                                
007902 01  FILLER              PIC X(16)  VALUE 'UT-AREA'.                      
008002*                                                                         
008104*01 AREA -COPY W47303    -PRE UT-                                         
008402     EJECT                                                                
008502*  ------------------------- DEALER-NET                                   
008602*01  FILLER -COPY WWDIST79                                                
008702     EJECT                                                                
008802*  ------------------------- ARBETSAREOR FÖR IMS-SEKTIONERNA              
008902*                                                                         
009002 01  FILLER                  PIC X(08)  VALUE 'IMS-WS'.                   
009102                                                                          
009202*  ------------------------- STATUSKOD FRÅN IMS                           
009302                                                                          
009402 01  STATUS-WS               PIC XX.                                      
009502     88  SEGMENT-FINNS                VALUE '  '.                         
009602     88  SEGMENT-SAKNAS               VALUE 'GE'.                         
009702     88  BASEN-SLUT                   VALUE 'GB'.                         
009802     88  IMS-EJ-OK                    VALUE 'XD'.                         
009902                                                                          
010002 01  GODK-STATUSKODER.                                                    
010102     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010202                                                                          
010302 01  SSA1                    PIC X(64).                                   
010402 01  SSA2                    PIC X(64).                                   
010502                                                                          
010602 01  FILLER                  PIC X(08)  VALUE 'DLI-NYCL'.                 
010702*  ---------------- NYCKLAR OCH SÖKFÄLT TILL DLI                          
010802 01  NYCKLAR-TILL-DLI.                                                    
010902     03  W-WDE4FSEQ-X.                                                    
011002         05  W-IDPRODNR            PIC S9(7)   COMP-3.                    
011102         05  W-IDKOLLI             PIC S9(5)   COMP-3.                    
011202                                                                          
011302     03  W-WDGXKEY-X.                                                     
011402         05  W-IDHTYP              PIC X(4)    VALUE '4321'.              
011502         05  FILLER                PIC X(26)   VALUE LOW-VALUE.           
011602                                                                          
011702     03  W-WDC701KY-X.                                                    
011802         05  W-IDDISTR-C7        PIC 9(4)    VALUE ZERO.                  
011902         05  W-IDKUNDNR-C7       PIC 9(7)    VALUE ZERO.                  
012002         05  W-IDBUNDLE-C7       PIC X(15).                               
012102         05  FILLER              REDEFINES W-IDBUNDLE-C7.                 
012202           07  W-IDORDER-C7      PIC 9(7).                                
012302           07  FILLER            PIC X(8).                                
012402     03  W-IDPRQUES-X.                                                    
012502         05  W-IDPRQUES          PIC 9(7)    VALUE ZERO.                  
012602*                                                                         
012702     EJECT                                                                
012802*01  -COPY W0003                                                          
012902     EJECT                                                                
013002 01  FILLER              PIC X(16)   VALUE 'DLI-IO-E401'.                 
013102 01  DLI-IO-E401.                                                         
013202*    03  -COPY WDE401                                                     
013302     EJECT                                                                
013402 01  FILLER              PIC X(16)   VALUE 'DLI-IO-E411'.                 
013502 01  DLI-IO-E411.                                                         
013602*    03  -COPY WDE411                                                     
013702     EJECT                                                                
013802 01  FILLER              PIC X(16)   VALUE 'DLI-IO-E421'.                 
013902 01  DLI-IO-E421.                                                         
014002*    03  -COPY WDE421                                                     
014102     EJECT                                                                
014202 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDG201'.               
014302 01  DLI-IO-WDG201.                                                       
014402*    03   -COPY WDGX01                                                    
014502     EJECT                                                                
014602 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDGX4322'.             
014702 01  DLI-IO-WDGX4322.                                                     
014802*    03   -COPY WDGX4322                                                  
014902     EJECT                                                                
015002 01  FILLER              PIC X(16)   VALUE 'DLI-IO-C701'.                 
015102 01  DLI-IO-C701.                                                         
015202*    03  -COPY WDC701                                                     
015302     EJECT                                                                
015402 01  FILLER              PIC X(16)   VALUE 'DLI-IO-C711'.                 
015502 01  DLI-IO-C711.                                                         
015602*    03  -COPY WDC711                                                     
015702     EJECT                                                                
015802 LINKAGE SECTION.                                                         
015902*01  -COPY W0008          -PRE WDE4-                                      
016002     05  FILLER       PIC X.                                              
016102     EJECT                                                                
016202*01  -COPY W0008          -PRE WDG2-                                      
016302     05  FILLER       PIC X.                                              
016402     EJECT                                                                
016502*01  -COPY W0008          -PRE WDC7-                                      
016602     05  FILLER       PIC X.                                              
016702     EJECT                                                                
016802 PROCEDURE DIVISION USING  WDE4-PCB WDG2-PCB WDC7-PCB.                    
016902                                                                          
017002     PERFORM A-INIT                                                       
017102     PERFORM IMS-GU-WDG201                                                
017202     PERFORM IMS-GNP-WDGX4322                                             
017302                                                                          
017402     PERFORM UNTIL SEGMENT-SAKNAS                                         
017502         MOVE 4322-IDPRODNR TO W-IDPRODNR                                 
017602         MOVE 4322-IDKOLLI  TO W-IDKOLLI                                  
017702         PERFORM IMS-GU-WDE411-FSEQ                                       
017802         IF SEGMENT-FINNS                                                 
017902           PERFORM IMS-GNP-WDE401                                         
018002                                                                          
018102           PERFORM B-REDIGERA-100-POST                                    
018202           PERFORM C-SKRIV-100-POST                                       
018302                                                                          
018402           PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                     
018502               PERFORM D-REDIGERA-111-POST                                
018602               PERFORM E-SKRIV-111-POST                                   
018702               PERFORM IMS-GN-WDE411-FSEQ                                 
018802           END-PERFORM                                                    
018902         END-IF                                                           
019002                                                                          
019103         PERFORM F-SKRIV-DLET-POST                                        
019202                                                                          
019302         PERFORM IMS-GNP-WDGX4322                                         
019402     END-PERFORM                                                          
019502                                                                          
019602     PERFORM Z-SLUT                                                       
019702     MOVE ZERO TO RETURN-CODE                                             
019802     GOBACK                                                               
019902     .                                                                    
020002     EJECT                                                                
020102 A-INIT SECTION.                                                          
020202     MOVE 'STA A-INIT-'          TO PGMPOS                                
020302                                                                          
020402     MOVE 'W4730100' TO POSTSUM-PROGNAMN                                  
020502     OPEN OUTPUT W47301                                                   
020602                 W47303                                                   
020702                                                                          
020802     MOVE 'END A-INIT-'          TO PGMPOS                                
020902     .                                                                    
021002     EJECT                                                                
021102 B-REDIGERA-100-POST SECTION.                                             
021202     MOVE 'STA B-REDIG'          TO PGMPOS                                
021302                                                                          
021402     MOVE '100'         TO 100-IDPTYP                                     
021502     MOVE KORD-IDDISTR  TO 100-IDDISTR                                    
021602                           DIST79-IDDISTR                                 
021702     MOVE KORD-IDKUNDNR TO 100-IDKUNDNR                                   
021802     MOVE KORD-IDORDNR5 TO 100-IDORDNR7                                   
021902     MOVE KORD-KDORDKL  TO 100-KDORDKL                                    
022002                                                                          
022102     MOVE 'END B-REDIG'          TO PGMPOS                                
022202     .                                                                    
022302     SKIP3                                                                
022402 C-SKRIV-100-POST SECTION.                                                
022502     MOVE 'STA C-SKRIV'          TO PGMPOS                                
022602                                                                          
022702     WRITE W47301-POST FROM 100-W4730100-CTX                              
022802                                                                          
022902     MOVE '100' TO POSTSUM-TRANSTYP                                       
023002     MOVE 'W47301' TO POSTSUM-FDNAMN                                      
023102     MOVE 'W47301D1' TO POSTSUM-DDNAMN2                                   
023202     CALL POSTSUM USING POSTSUM-PARM                                      
023302                                                                          
023402     MOVE 'END C-SKRIV'          TO PGMPOS                                
023502     .                                                                    
023602     EJECT                                                                
023702 D-REDIGERA-111-POST SECTION.                                             
023802     MOVE 'STA D-REDIG'          TO PGMPOS                                
023902                                                                          
024002     MOVE '111'           TO 111-IDPTYP                                   
024102     MOVE ORAD-IDARTNR    TO 111-IDARTNR                                  
024202     MOVE 4322-IDKOLLI    TO 111-IDKOLLI                                  
024302     IF ORAD-IDKUNDRF-RO = '00000     '                                   
024402       MOVE KORD-IDORDNR5 TO 111-IDORDNR7                                 
024502     ELSE                                                                 
024602       MOVE ORAD-IDKUNDRF-RO (1:5)                                        
024702                          TO 111-IDORDNR7                                 
024802     END-IF                                                               
024902     PERFORM IMS-GNP-WDE421                                               
025002     MOVE KKOLLI-KVLEVART TO 111-KVLEVART                                 
025102     MOVE ORAD-KDORDKL    TO 111-KDORDKL                                  
025202     MOVE ORAD-BERADREF   TO 111-BERADREF                                 
025302     IF DIST79-DEALER-PRICE                                               
025402       IF ORAD-PRARTNTO-LOCPREL > 0                                       
025502         PERFORM DA-HAMTA-DEALERPRIS                                      
025602       ELSE                                                               
025702         MOVE ORAD-PRARTNTO-LOC TO 111-PRARTNTO                           
025802       END-IF                                                             
025902     ELSE                                                                 
026102       IF DIST79-ECOM-PRICE                                               
026202         MOVE ORAD-PRARTNTO-LOC TO 111-PRARTNTO                           
026302       ELSE                                                               
026402         IF ORAD-KDPRTYP = 'T'                                            
026502           MOVE ZERO    TO 111-PRARTNTO                                   
026602         ELSE                                                             
026702           IF DIST79-DEALER-PRICE OR                                      
026902              DIST79-ECOM-PRICE                                           
027002             MOVE ORAD-PRARTNTO-LOC TO 111-PRARTNTO                       
027102           ELSE                                                           
027202             MOVE ORAD-PRARTNTO TO 111-PRARTNTO                           
027302           END-IF                                                         
027402         END-IF                                                           
027502       END-IF                                                             
027602     END-IF                                                               
027702     IF ORAD-TIRODAT > ZERO                                               
027802        MOVE 1            TO 111-KDRO                                     
027902     ELSE                                                                 
028002        MOVE ZERO         TO 111-KDRO                                     
028102     END-IF                                                               
028202                                                                          
028302     MOVE 'END D-REDIG'          TO PGMPOS                                
028402     .                                                                    
028502     SKIP3                                                                
028602 DA-HAMTA-DEALERPRIS SECTION.                                             
028702     MOVE KORD-IDDISTR    TO W-IDDISTR-C7                                 
028802     MOVE KORD-IDKUNDNR   TO W-IDKUNDNR-C7                                
028902     MOVE SPACE           TO W-IDBUNDLE-C7                                
029002     MOVE ZERO            TO W-IDORDER-C7(1:2)                            
029102     IF ORAD-IDKUNDRF-RO = '00000     '                                   
029202       MOVE KORD-IDORDNR5 TO W-IDORDER-C7(3:5)                            
029302     ELSE                                                                 
029402       MOVE ORAD-IDKUNDRF-RO (1:5)                                        
029502                          TO W-IDORDER-C7(3:5)                            
029602     END-IF                                                               
029702     MOVE ORAD-IDPRQUES   TO W-IDPRQUES                                   
029802     PERFORM IMS-GU-WDC711                                                
029902     IF SEGMENT-FINNS                                                     
030002       IF LPRQ-KDPRSTA = 'A' OR 'M'                                       
030102         MOVE LPRQ-PRARTNTO-LOC  TO 111-PRARTNTO                          
030202       ELSE                                                               
030302         MOVE ZERO               TO 111-PRARTNTO                          
030402       END-IF                                                             
030502     ELSE                                                                 
030602       MOVE ZERO                 TO 111-PRARTNTO                          
030702     END-IF                                                               
030802     .                                                                    
030902     SKIP3                                                                
031002 E-SKRIV-111-POST SECTION.                                                
031102     MOVE 'STA E-SKRIV'          TO PGMPOS                                
031202                                                                          
031302     WRITE W47301-POST FROM 111-W4730111-CTX                              
031402                                                                          
031502     MOVE '111' TO POSTSUM-TRANSTYP                                       
031602     MOVE 'W47301' TO POSTSUM-FDNAMN                                      
031702     MOVE 'W47301D1' TO POSTSUM-DDNAMN2                                   
031802     CALL POSTSUM USING POSTSUM-PARM                                      
031902                                                                          
032002     MOVE 'END E-SKRIV'          TO PGMPOS                                
032102     .                                                                    
032202     EJECT                                                                
032303 F-SKRIV-DLET-POST SECTION.                                               
032402                                                                          
032502     MOVE 4322-IDPRODNR     TO UT-IDPRODNR                                
032602     MOVE 4322-IDKOLLI      TO UT-IDKOLLI                                 
032702     WRITE W47303-POST FROM UT-AREA                                       
032802                                                                          
032902     MOVE 'G202' TO POSTSUM-TRANSTYP                                      
033002     MOVE 'W47303' TO POSTSUM-FDNAMN                                      
033102     MOVE 'W47301D2' TO POSTSUM-DDNAMN2                                   
033202     CALL POSTSUM USING POSTSUM-PARM                                      
033502     .                                                                    
033602     EJECT                                                                
033702 Z-SLUT SECTION.                                                          
033902                                                                          
034002     CLOSE W47301                                                         
034102           W47303                                                         
034202                                                                          
034302     MOVE 'S' TO POSTSUM-OPKOD                                            
034402     CALL POSTSUM USING POSTSUM-PARM                                      
034702     .                                                                    
034802     EJECT                                                                
034902 IMS-GU-WDG201 SECTION.                                                   
035002     SKIP2                                                                
035102     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
035202            DELIMITED BY SIZE INTO SSA1                                   
035302     MOVE '    ' TO GODK-STATUSKODER                                      
035402     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDG201 SSA1                    
035502     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
035602     PERFORM IMS-STATUSKONTROLL                                           
035702     .                                                                    
035802     SKIP3                                                                
035902 IMS-GNP-WDGX4322 SECTION.                                                
036002     SKIP2                                                                
036102     MOVE 'WDG202 ' TO SSA1                                               
036202     MOVE '  GE' TO GODK-STATUSKODER                                      
036302     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX4322 SSA1                 
036402     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
036502     PERFORM IMS-STATUSKONTROLL                                           
036602     .                                                                    
036702     SKIP3                                                                
037702 IMS-GU-WDE411-FSEQ SECTION.                                              
037802     SKIP2                                                                
037902     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
038002            DELIMITED BY SIZE INTO SSA1                                   
038102     MOVE '  GE' TO GODK-STATUSKODER                                      
038202     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E411 SSA1                      
038302     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
038402     PERFORM IMS-STATUSKONTROLL                                           
038502     .                                                                    
038602     SKIP3                                                                
038702 IMS-GN-WDE411-FSEQ SECTION.                                              
038802     SKIP2                                                                
038902     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
039002            DELIMITED BY SIZE INTO SSA1                                   
039102     MOVE '  GEGB' TO GODK-STATUSKODER                                    
039202     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E411 SSA1                      
039302     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
039402     PERFORM IMS-STATUSKONTROLL                                           
039502     .                                                                    
039602     EJECT                                                                
039702 IMS-GNP-WDE401 SECTION.                                                  
039802     SKIP2                                                                
039902     MOVE 'WDE401 ' TO SSA1                                               
040002     MOVE '  ' TO GODK-STATUSKODER                                        
040102     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E401 SSA1                     
040202     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
040302     PERFORM IMS-STATUSKONTROLL                                           
040402     .                                                                    
040502     SKIP2                                                                
040602 IMS-GNP-WDE421 SECTION.                                                  
040702     SKIP2                                                                
040802     STRING 'WDE421  (WDE421KY =' W-WDE4FSEQ-X ')'                        
040902            DELIMITED BY SIZE INTO SSA1                                   
041002     MOVE '  ' TO GODK-STATUSKODER                                        
041102     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E421 SSA1                     
041202     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
041302     PERFORM IMS-STATUSKONTROLL                                           
041402     .                                                                    
041502     EJECT                                                                
041602 IMS-GU-WDC711 SECTION.                                                   
041702     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
041802          DELIMITED BY SIZE INTO SSA1                                     
041902     STRING 'WDC711  (IDPRQUES =' W-IDPRQUES-X ')'                        
042002          DELIMITED BY SIZE INTO SSA2                                     
042102     MOVE '  GE' TO GODK-STATUSKODER                                      
042202     CALL CBLTDLI USING GU WDC7-PCB DLI-IO-C711 SSA1 SSA2                 
042302     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
042402     PERFORM IMS-STATUSKONTROLL                                           
042502     .                                                                    
042602     SKIP3                                                                
042702 IMS-STATUSKONTROLL SECTION.                                              
042802     SKIP2                                                                
042902     SET STATUS-IX TO 1                                                   
043002     SEARCH GODK-STATUS AT END CALL FELLOG                                
043102     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE END-SEARCH         
044002     .                                                                    
