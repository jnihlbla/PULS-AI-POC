000100*COMPOPT XPOSIX=YES                                                       
000200                                                                          
000300 ID  DIVISION.                                                            
000400                                                                          
000500 PROGRAM-ID.    WZ142000.                                                 
000600                                                                          
000700 AUTHOR.        CLAES OLANDER.                                            
000800                                                                          
000900 DATE-WRITTEN.  NOV 2005.                                                 
001000                                                                          
001100 DATE-COMPILED.                                                           
001200*                                                                         
001300*****************************************************************         
001400*                                                                         
001500*              DESCRIPTION.                                               
001600*              ************                                               
001700*                                                                         
001800* PURPOSE:     The directory of a specific folder in a                    
001900*              specific mailbox is listed for mails                       
002000*                                                                         
002100*              Information about these mails plus                         
002200*              information about these mails are sent to an               
002300*              exit-program.                                              
002400*****                                                                     
002500*                                                                         
002600*                                                                         
002700* SUBPROGRAMS: FMAILAPI    Interface to mail-api.                         
002800*                                                                         
002900*                                                                         
003000*****                                                                     
003100*                                                                         
003200* INPUT:       WZ1420D0.                                                  
003300*              fixed-blocked, LRECL=80.                                   
003400*                                                                         
003500*              Must not contain more than 20 card-images,                 
003600*              otherwise we abend U0016.                                  
003700*                                                                         
003800*              Contains keywords. A keyword can be in any                 
003900*              of the WZ1420D0-card-image, provided that the              
004000*              keyword-operand ands before column 72.                     
004100*                                                                         
004200*              The keyword-operand must Immidiately follow                
004300*              the last character in the keyword, that is                 
004400*              the character '='.                                         
004500*                                                                         
004600*              The end of a keyword is idicated by a character            
004700*              containing a value less than X'41', normally               
004800*              a blank                                                    
004900*                                                                         
005000*              The follwing 3 keywords are supported:                     
005100*                                                                         
005200*                                                                         
005300*              DATA=                                                      
005400*                                                                         
005500*                  Contains 50 characters to be read by                   
005600*                  the EXITPGM.                                           
005700*                                                                         
005800*              EXITPGM=                                                   
005900*                                                                         
006000*                  The keyword operand contains the name                  
006100*                  of an exit-program. This program must be               
006200*                  on the steplib-linklist concatinate.                   
006300*                                                                         
006400*              FOLDER=                                                    
006500*                                                                         
006600*                  The keyword operand contains a FOLDER name             
006700*                  in the outllok mailbox beeing read.                    
006800*                                                                         
006900*                  The directory of that folder will be listed            
007000*                  and each directory will latwr be sent to an            
007100*                  exit-program, that will deside if the                  
007200*                  directory information will be saved or not,            
007300*                  and if to be saved, on whitsh dd-name.                 
007400*                                                                         
007500*****                                                                     
007600*                                                                         
007700* OUTPUT:      WZ1420DN,  N = 1-9 or A                                    
007800*                                                                         
007900*              Fixed-blocked, LRECL=728                                   
008000*                                                                         
008100*              Contains directory -entry information.            s        
008200*                                                                         
008300*****                                                                     
008400*                                                                         
008500* LOGICS:      This program is controled from 2 sources:                  
008600*                                                                         
008700*              A. The parm-value                                          
008800*                                                                         
008900*                 It contains 2 positional value, separated               
009000*                 by a ',':                                               
009100*                                                                         
009200*                                                                         
009300*                 MAXRC   2-digits number, containing the highest         
009400*                         acceptable return-code from FFMAILAI.           
009500*                         If omitted, the value is taken from             
009600*                         the load-module, the name of which is           
009700*                         to befound in the second positional             
009800*                         field.                                          
009900*                                                                         
010000*                 MODNM   Name of a load-module, containing               
010100*                         the outlook user and password.      .           
010200*                                                                         
010300*                         the load-module, the name of which is           
010400*                         to befound in the second positional             
010500*                         field.                                          
010600*                                                                         
010700*                                                                         
010800*              B. The SYSIN-file                                          
010900*                                                                         
011000*                 It contains 3 card-images with key-words,               
011100*                 each terminated by a'='                                 
011200*                                                                         
011300*                                                                         
011400*                 DATA    50 characters field, containing data to         
011500*                         be conwayd to and used by EXIPGM.               
011600*                                                                         
011700*                 EXITPGM Name of an exit-pgm. This program               
011800*                         decides if to store and where to                
011900*                         store mails from the mailbox.                   
012000*                         10 files are available for storage.             
012100*                                                                         
012200*                         The exit-pgm also disides what files            
012300*                         of the 10, to be read afterwards and            
012400*                         and used as input the delete-commands           
012500*                         against the mailbox whith the outlook-          
012600*                         id, given in the MODNM-module.                  
012700*                                                                         
012800*                 FOLDER  The name of the folder to be read.              
012900*                                                                         
013000*                                                                         
013100*              The directory-information about all mailes,                
013200*              in the folder specified, are read, using FMAILAPI.         
013300*                                                                         
013400*              This informatian for every mail and a vector               
013500*              with 10 delete-flags is sent to EXITPGM.                   
013600*                                                                         
013700*              At return, EXITPGM has given 1 of the following            
013800*              values in A-RETCODE:                                       
013900*                                                                         
014000*                                                                         
014100*              01 <= A-RETCODE <= 10                                      
014200*                  Means that the mail-directory-information              
014300*                  should be stored in a corresponding file.              
014400*                                                                         
014500*              A-RETCODE = 96                                             
014600*                  Means, skip this mail-information and                  
014700*                  give me the information about the next                 
014800*                  mail.                                                  
014900*                                                                         
015000*              A-RETCODE = 97                                             
015100*                  Means, skip this mail-information and                  
015200*                  information for all subsequent mails.                  
015300*                                                                         
015400*              A-RETCODE = 98                                             
015500*                  Means, EXITPGM has found an error and want             
015600*                  this program to abend U0100.                           
015700*                                                                         
015800*                                                                         
015900*              After the last mail has been examined by EXITPGM,          
016000*              this program is invoked a last time with                   
016100*              A-RETCODE =99 to make it possible for EXITPGM              
016200*              to clean-up.                                               
016300*                                                                         
016400*              Then the vector with 10 possible delete-flags              
016500*              is parsed.                                                 
016600*                                                                         
016700*              For each delete-flag with the value 'D'.                   
016800*              the corresponding storage-file is read and                 
016900*              for each record we issue a delete-call aginst              
017000*              outlook via FMAILAPI.                                      
017100*                                                                         
017200*****                                                                     
017300*                                                                         
017400* ABENDS:      U0016 (without dump), always preceeded by one              
017500*              of the messages M001 - M013 and then always                
017600*              message M999.                                              
017700*                                                                         
017800*              WZ142000 M001 NOT AUTORIZED TO RUN FMAILAPI                
017900*                                                                         
018000*              WZ142000 M002 MORE THAN 20 CARDS ON WZ1420D0               
018100*                                                                         
018200*              WZ142000 M003 DATA-KEYWORD MISSING                         
018300*                                                                         
018400*              WZ142000 M004 EXITPGM-KEYWORD MISSING                      
018500*                                                                         
018600*              WZ142000 M005 FOLDER-KEYWORD MISSING                       
018700*                                                                         
018800*              WZ142000 M006 RC > 00 FROM FMAILAPI-LIST                   
018900*                                                                         
019000*              WZ142000 M007 ABEND-REQUEST FROM EXITPGM                   
019100*                                                                         
019200*              WZ142000 M008 INVALID RETCODE FROM EXITPGM                 
019300*                                                                         
019400*              WZ142000 M009 INVALID RETCODE FROM EXITPGM                 
019500*                                                                         
019600*              WZ142000 M010 PROGRAM-ERROR                                
019700*                                                                         
019800*              WZ142000 M011 PROGRAM-ERROR                                
019900*                                                                         
020000*              WZ142000 M012 RC > 00 FROM FMAILAPI-DELETE                 
020100*                                                                         
020200*              WZ142000 M013 RC > 00 FROM FMAILAPI-CLOSE                  
020300*                                                                         
020400*              WZ142000 M999 *** ABNORMAL TERMINATION ***'                
020500*                                                                         
020600*****                                                                     
020700*                                                                         
020800* REGION:      Because the mail-api takes a lot of memory                 
020900*              the region-pareter for                                     
021000*              this program / its procedure / job-card                    
021100*              must be set to at least 'REGION=120M'  !!!!!               
021200*                                                                         
021300*****                                                                     
021400*                                                                         
021500* RETURN-CODE:  Always X'0000'                                            
021600*                                                                         
021700*****************************************************************         
021800*                                                                         
021900 ENVIRONMENT DIVISION.                                                    
022000*                                                                         
022100 INPUT-OUTPUT SECTION.                                                    
022200                                                                          
022300 FILE-CONTROL.                                                            
022400                                                                          
022500*    -- Input                                                             
022600     SELECT WZ1420D0                   ASSIGN TO WZ1420D0.                
022700                                                                          
022800*    -- Output                                                            
022900     SELECT WZ1420D1                   ASSIGN TO WZ1420D1.                
023000                                                                          
023100     SELECT WZ1420D2                   ASSIGN TO WZ1420D2.                
023200                                                                          
023300     SELECT WZ1420D3                   ASSIGN TO WZ1420D3.                
023400                                                                          
023500     SELECT WZ1420D4                   ASSIGN TO WZ1420D4.                
023600                                                                          
023700     SELECT WZ1420D5                   ASSIGN TO WZ1420D5.                
023800                                                                          
023900     SELECT WZ1420D6                   ASSIGN TO WZ1420D6.                
024000                                                                          
024100     SELECT WZ1420D7                   ASSIGN TO WZ1420D6.                
024200                                                                          
024300     SELECT WZ1420D8                   ASSIGN TO WZ1420D8.                
024400                                                                          
024500     SELECT WZ1420D9                   ASSIGN TO WZ1420D9.                
024600                                                                          
024700     SELECT WZ1420DA                   ASSIGN TO WZ1420DA.                
024800                                                                          
024900                                                                          
025000 DATA DIVISION.                                                           
025100*                                                                         
025200 FILE SECTION.                                                            
025300                                                                          
025400 FD  WZ1420D0                                                             
025500     RECORDING       F                                                    
025600     BLOCK CONTAINS  0                                                    
025700     .                                                                    
025800                                                                          
025900 01     WZ1420D0-POST    PIC X(80).                                       
026000                                                                          
026100                                                                          
026200 FD  WZ1420D1                                                             
026300     RECORDING       F                                                    
026400     BLOCK CONTAINS  0                                                    
026500     .                                                                    
026600                                                                          
026700 01     WZ1420D1-POST    -COPY WZ1420NN -L                                
026800                                                                          
026900                                                                          
027000 FD  WZ1420D2                                                             
027100     RECORDING       F                                                    
027200     BLOCK CONTAINS  0                                                    
027300     .                                                                    
027400                                                                          
027500 01     WZ1420D2-POST    -COPY WZ1420NN -L                                
027600                                                                          
027700                                                                          
027800 FD  WZ1420D3                                                             
027900     RECORDING       F                                                    
028000     BLOCK CONTAINS  0                                                    
028100     .                                                                    
028200                                                                          
028300 01     WZ1420D3-POST    -COPY WZ1420NN -L                                
028400                                                                          
028500                                                                          
028600 FD  WZ1420D4                                                             
028700     RECORDING       F                                                    
028800     BLOCK CONTAINS  0                                                    
028900     .                                                                    
029000                                                                          
029100 01     WZ1420D4-POST    -COPY WZ1420NN -L                                
029200                                                                          
029300                                                                          
029400 FD  WZ1420D5                                                             
029500     RECORDING       F                                                    
029600     BLOCK CONTAINS  0                                                    
029700     .                                                                    
029800                                                                          
029900 01     WZ1420D5-POST    -COPY WZ1420NN -L                                
030000                                                                          
030100                                                                          
030200 FD  WZ1420D6                                                             
030300     RECORDING       F                                                    
030400     BLOCK CONTAINS  0                                                    
030500     .                                                                    
030600                                                                          
030700 01     WZ1420D6-POST    -COPY WZ1420NN -L                                
030800                                                                          
030900                                                                          
031000 FD  WZ1420D7                                                             
031100     RECORDING       F                                                    
031200     BLOCK CONTAINS  0                                                    
031300     .                                                                    
031400                                                                          
031500 01     WZ1420D7-POST    -COPY WZ1420NN -L                                
031600                                                                          
031700                                                                          
031800 FD  WZ1420D8                                                             
031900     RECORDING       F                                                    
032000     BLOCK CONTAINS  0                                                    
032100     .                                                                    
032200                                                                          
032300 01     WZ1420D8-POST    -COPY WZ1420NN -L                                
032400                                                                          
032500                                                                          
032600 FD  WZ1420D9                                                             
032700     RECORDING       F                                                    
032800     BLOCK CONTAINS  0                                                    
032900     .                                                                    
033000                                                                          
033100 01     WZ1420D9-POST    -COPY WZ1420NN -L                                
033200                                                                          
033300                                                                          
033400 FD  WZ1420DA                                                             
033500     RECORDING       F                                                    
033600     BLOCK CONTAINS  0                                                    
033700     .                                                                    
033800                                                                          
033900 01     WZ1420DA-POST    -COPY WZ1420NN -L                                
034000                                                                          
034100                                                                          
034200*                                                                         
034300 WORKING-STORAGE SECTION.                                                 
034400                                                                          
034500 01     ERRMSG           PIC X(80).                                       
034600                                                                          
034700 01     EOF-FLGS         PIC X(01).                                       
034800   88   EOF-YES          VALUE 'Y'.                                       
034900   88   EOF-NOT          VALUE 'N'.                                       
035000                                                                          
035100 01     OP2-FLGS         PIC X(01).                                       
035200   88   OP2-OPEN         VALUE 'Y'.                                       
035300   88   OP2-NOT-OPEN     VALUE 'N'.                                       
035400                                                                          
035500 01     QUIT-FLGS        PIC X(01).                                       
035600   88   QUIT-YES         VALUE 'Y'.                                       
035700   88   QUIT-NO          VALUE 'N'.                                       
035800                                                                          
035900 01     SW-CLOSE         PIC S9(8) BINARY.                                
036000                                                                          
036100 01     SLASK            PIC S9(4) BINARY.                                
036200                                                                          
036300 01     PNTR             USAGE IS POINTER.                                
036400                                                                          
036500*                                        -- A-AREA                        
036600*       -COPY WZ1420NN  -PRE A-                                           
036700*                                                                         
036800                                                                          
036900*                                        -- S-AREA                        
037000*       -COPY WZ1420NN  -PRE S-                                           
037100*                                                                         
037200                                                                          
037300 01     B-AREA           PIC X(10)       VALUE SPACES.                    
037400 01     FILLER REDEFINES B-AREA.                                          
037500   03   B-BYTE   OCCURS 10 INDEXED BY B-IX                                
037600                         PIC X(01).                                       
037700                                                                          
037800 01     C-TYPE           PIC S9(9) BINARY.                                
037900                                                                          
038000 01     CT-AREA.                                                          
038100   03   CT-CARD OCCURS  20 INDEXED BY CT-IX                               
038200                         PIC X(80).                                       
038300 01     FILLER   REDEFINES CT-AREA.                                       
038400   03   CT-BYTE OCCURS 1600 INDEXED BY CT-IX2                             
038500                         PIC X(01).                                       
038600                                                                          
038700 01     CTINDEX          PIC S9(9) BINARY VALUE +0.                       
038800                                                                          
038900 01     EX-AREA.                                                          
039000   03   EX-BYTE  OCCURS  1600 TIMES INDEXED BY EX-IX                      
039100                         PIC X(01).                                       
039200                                                                          
039300 01     KW-AREA.                                                          
039400   03   KW-DATA          PIC X(05)        VALUE 'DATA='.                  
039500   03   KW-EXITPGM       PIC X(08)        VALUE 'EXITPGM='.               
039600   03   KW-FOLDER        PIC X(07)        VALUE 'FOLDER='.                
039700                                                                          
039800 01     OP-AREA          PIC X(10)        VALUE ALL 'N'.                  
039900 01     FILLER   REDEFINES OP-AREA.                                       
040000   03   OP-BYTE OCCURS 10 TIMES INDEXED BY OP-IX                          
040100                         PIC X(01).                                       
040200                                                                          
040300 01  WZ11MSEC            PIC X(08)        VALUE 'WZ11MSEC'.               
040400                                                                          
040500*  -COPY WZ11MSEC                                                         
040600                                                                          
040700                                                                          
040800 01  ABEND               PIC X(08)        VALUE 'ABEND   '.               
040900                                                                          
041000 77  RKOD-ABEND-UTAN-DUMP PIC S9(4)  COMP VALUE +16.                      
041100 77  RKOD-ABEND-MED-DUMP  PIC S9(4)  COMP VALUE +1000.                    
041200                                                                          
041300 01  RC                  PIC S9(4).                                       
041400                                                                          
041500                                                                          
041600 01  WZ20DAYS            PIC X(08)        VALUE 'WZ20DAYS'.               
041700                                                                          
041800*01  -COPY WZ20DAYS                                                       
041900                                                                          
042000                                                                          
042100 01  W009WAIT            PIC X(08)        VALUE 'W009WAIT'.               
042200                                                                          
042300*    -- PARAMETER TO W009WAIT - IN HUNDREDS OF SECONDS                    
042400 01  20-SECONDS          PIC S9(9)   COMP VALUE +2000.                    
042500                                                                          
042600                                                                          
042700 01     FMAILAPI         PIC X(08)        VALUE 'FMAILAPI'.               
042800                                                                          
042900 01  RETRIEVE-LIST.                                                       
043000     03  RETRIEVE-LIST-ENTRY  OCCURS 1000 INDEXED BY RETRIEVE-IX          
043100                                                     RETRIEVE-IX2.        
043200       05  RETRIEVE-REFID                                                 
043300                         PIC X(016).                                      
043400       05  RETRIEVE-SUBJECT                                               
043500                         PIC X(256).                                      
043600       05  RETRIEVE-MAIL-ADDR                                             
043700                         PIC X(256).                                      
043800       05  RETRIEVE-DATE                                                  
043900                         PIC X(032).                                      
044000       05  RETRIEVE-STATUS                                                
044100                         PIC X(032).                                      
044200       05  RETRIEVE-CONTENT-TYPE                                          
044300                         PIC X(032).                                      
044400*                                                                         
044500*       -COPY FMAILRCV                                                    
044600*                                                                         
044700*       -COPY FMAILCOM                                                    
044800*                                                                         
044900 LINKAGE SECTION.                                                         
045000                                                                          
045100 01     PARM-AREA.                                                        
045200   03   PARM-LENGTH      PIC S9(4) BINARY.                                
045300   03   PARM-MODNM       PIC X(8).                                        
045400                                                                          
045500 01     LI-AREA          PIC X(12).                                       
045600                                                                          
045700 PROCEDURE DIVISION USING PARM-AREA.                                      
045800*                                                                         
045900 MAIN SECTION.                                                            
046000                                                                          
046100     MOVE '00'                        TO MSEC-MAXRC                       
046200     MOVE PARM-MODNM(1:PARM-LENGTH)   TO MSEC-MODNM                       
046300                                                                          
046400     CALL WZ11MSEC              USING MSEC-AREA                           
046500                                                                          
046600     IF  MSEC-ANSWN                                                       
046700         MOVE '001 FMAILAPI PARAMETERS COULD NOT BE RETRIEVED'            
046800             TO ERRMSG                                                    
046900                                                                          
047000         PERFORM S01-ABEND                                                
047100                                                                          
047200     END-IF                                                               
047300                                                                          
047400     MOVE LOW-VALUES            TO RETRIEVE-LIST                          
047500                                                                          
047600     PERFORM A-CTINPUT                                                    
047700                                                                          
047800     PERFORM B-PARSE                                                      
047900                                                                          
048000     PERFORM C-CHECK                                                      
048100                                                                          
048200     PERFORM D-LISTREQ                                                    
048300                                                                          
048400     PERFORM E-RETRENTRY                                                  
048500                                                                          
048600     SET B-IX                   TO  +1                                    
048700                                                                          
048800     PERFORM UNTIL              B-IX > +10                                
048900                                                                          
049000         IF  B-BYTE (B-IX) = 'D'                                          
049100                                                                          
049200             PERFORM F-DELETE                                             
049300                                                                          
049400         END-IF                                                           
049500         SET B-IX                   UP  BY +1                             
049600     END-PERFORM                                                          
049700                                                                          
049800     PERFORM G-CLOSREQ                                                    
049900                                                                          
050000     IF  B-AREA NOT > SPACES                                              
050100         MOVE ZEROES                TO  RETURN-CODE                       
050200     ELSE                                                                 
050300         MOVE RC                    TO  RETURN-CODE                       
050400     END-IF                                                               
050500                                                                          
050600     GOBACK                                                               
050700     .                                                                    
050800*                                                                         
050900 A-CTINPUT SECTION.                                                       
051000*                                                                         
051100*****************************************************************         
051200*                                                                         
051300*    READS THE WHOLE FILE WZ1420D0 INTO CORE                              
051400*                                                                         
051500*****************************************************************         
051600*                                                                         
051700     OPEN INPUT WZ1420D0                                                  
051800                                                                          
051900     PERFORM AA-CTREAD                                                    
052000                                                                          
052100     COMPUTE SLASK = (LENGTH OF CT-AREA / LENGTH OF CT-CARD)              
052200     SET CT-IX                  TO +1                                     
052300     SET EOF-NOT                TO TRUE                                   
052400                                                                          
052500     PERFORM UNTIL EOF-YES OR CT-IX > SLASK                               
052600         MOVE WZ1420D0-POST         TO CT-CARD (CT-IX)                    
052700         SET CT-IX                  UP BY +1                              
052800                                                                          
052900         PERFORM AA-CTREAD                                                
053000                                                                          
053100     END-PERFORM                                                          
053200                                                                          
053300     IF  CT-IX > SLASK                                                    
053400         MOVE '002 MORE THAN 20 CARDS ON WZ1420D0'                        
053500             TO ERRMSG                                                    
053600                                                                          
053700         PERFORM S01-ABEND                                                
053800                                                                          
053900     END-IF                                                               
054000                                                                          
054100     CLOSE WZ1420D0                                                       
054200     .                                                                    
054300*                                                                         
054400 AA-CTREAD SECTION.                                                       
054500*                                                                         
054600*****************************************************************         
054700*                                                                         
054800*    READS WZ1420D0.                                                      
054900*                                                                         
055000*****************************************************************         
055100*                                                                         
055200     READ WZ1420D0                                                        
055300     AT  END                                                              
055400         SET EOF-YES                TO TRUE                               
055500     NOT AT  END                                                          
055600         MOVE SPACES                TO  WZ1420D0-POST (72 : 09)           
055700     END-READ                                                             
055800     .                                                                    
055900*                                                                         
056000 B-PARSE SECTION.                                                         
056100*                                                                         
056200*****************************************************************         
056300*                                                                         
056400*    PARSES WZ1420D0 FOR THE FOLLOWING KEYWORDS:                          
056500*                                                                         
056600*    USER=, PASSWORD=, SERVERR=,                                          
056700*    FOLDER= AND EXITPGM=                                                 
056800*                                                                         
056900*    THE CORRESPONDING KEYWORD-OPERANDS ARE EXTRACTED USING               
057000*    THE SECTION 'BA-EXTRACT' AND AFTERWARDS COPIED TO THE                
057100*    CORRESPONDING A-AREA FIELDS.                                         
057200*                                                                         
057300*****************************************************************         
057400*                                                                         
057500     SET CT-IX2                 TO +1                                     
057600                                                                          
057700     PERFORM UNTIL CT-IX2 > LENGTH OF CT-AREA                             
057800         SET PNTR                                                         
057900             TO ADDRESS OF CT-BYTE (CT-IX2)                               
058000         SET ADDRESS OF LI-AREA     TO PNTR                               
058100         IF  LI-AREA (1 : 05) = KW-DATA                                   
058200             SET CT-IX2                 UP BY +05                         
058300                                                                          
058400             PERFORM BA-EXRACT                                            
058500                                                                          
058600             MOVE EX-AREA               TO A-DATA                         
058700         END-IF                                                           
058800         IF  LI-AREA (1 : 08) = KW-EXITPGM                                
058900             SET CT-IX2                 UP BY +08                         
059000                                                                          
059100             PERFORM BA-EXRACT                                            
059200                                                                          
059300             MOVE EX-AREA               TO A-EXITPGM                      
059400         END-IF                                                           
059500         IF  LI-AREA (1 : 07) = KW-FOLDER                                 
059600             SET CT-IX2                 UP BY +07                         
059700                                                                          
059800             PERFORM BA-EXRACT                                            
059900                                                                          
060000             MOVE EX-AREA               TO A-FOLDER                       
060100         END-IF                                                           
060200         SET  CT-IX2                UP BY +1                              
060300     END-PERFORM                                                          
060400     .                                                                    
060500*                                                                         
060600 BA-EXRACT SECTION.                                                       
060700*                                                                         
060800*****************************************************************         
060900*                                                                         
061000*    EXTRACTS THE KEYWORD-OPERANDS FOR THE FOLLOWING KEYWORDS:            
061100*                                                                         
061200*    USER=, PASSWORD1=, SERVER=,                                          
061300*    FOLDER= AND EXITPGM=                                                 
061400*                                                                         
061500*    FOR TEMPORARY STORING IN EX-AREA.                                    
061600*                                                                         
061700*****************************************************************         
061800*                                                                         
061900     MOVE SPACES                TO EX-AREA                                
062000     SET EX-IX                  TO +1                                     
062100                                                                          
062200     SET QUIT-NO                TO TRUE                                   
062300     PERFORM UNTIL CT-IX2 > LENGTH OF CT-AREA - 20                        
062400         OR QUIT-YES                                                      
062500         SET CTINDEX                        TO  CT-IX2                    
062600         IF  CT-AREA (CTINDEX : 7)          = ' DATA='                    
062700             SET QUIT-YES                       TO  TRUE                  
062800         ELSE                                                             
062900             IF  CT-AREA (CTINDEX : 9)          = ' EXITPGM='             
063000                 SET QUIT-YES                       TO  TRUE              
063100             ELSE                                                         
063200                 IF  CT-AREA (CTINDEX : 8)          = ' FOLDER='          
063300                     SET QUIT-YES                       TO  TRUE          
063400                 ELSE                                                     
063500                     MOVE CT-BYTE (CT-IX2)                                
063600                        TO EX-BYTE (EX-IX)                                
063700                     SET EX-IX                      UP BY +1              
063800                     SET CT-IX2                     UP BY +1              
063900                 END-IF                                                   
064000             END-IF                                                       
064100         END-IF                                                           
064200     END-PERFORM                                                          
064300                                                                          
064400     SET CT-IX2                     DOWN BY +1                            
064500     .                                                                    
064600*                                                                         
064700 C-CHECK SECTION.                                                         
064800*                                                                         
064900*****************************************************************         
065000*                                                                         
065100*    CHECKS THAT ALL THE FOLLOWING KEYWORDS EXISTS ON CTLFILE:            
065200*                                                                         
065300*    USER=,  PASSWORD1=, SERVER=,                                         
065400*    FOLDER= AND EXITPGM=                                                 
065500*                                                                         
065600*    IF NOT ALL PRESENT, WE ABEND U0100                                   
065700*                                                                         
065800*    WE ALSO CHECKS THAT INTERVAL IS NUMEIC, POSSITIVE AND                
065900*    CONSISTS OF EXACTLY 5 DIGITS, OTHERWISE WE ABEND U0100.              
066000*                                                                         
066100*****************************************************************         
066200*                                                                         
066300     IF  A-DATA NOT > SPACES                                              
066400         MOVE '003 DATA-KEYWORD MISSING'                                  
066500             TO ERRMSG                                                    
066600                                                                          
066700         PERFORM S01-ABEND                                                
066800                                                                          
066900     END-IF                                                               
067000     IF  A-EXITPGM NOT > SPACES                                           
067100         MOVE '004 EXITPGM-KEYWORD MISSING'                               
067200             TO ERRMSG                                                    
067300                                                                          
067400         PERFORM S01-ABEND                                                
067500                                                                          
067600     END-IF                                                               
067700     IF  A-FOLDER NOT > SPACES                                            
067800         MOVE '005 FOLDER-KEYWORD MISSING'                                
067900             TO ERRMSG                                                    
068000                                                                          
068100         PERFORM S01-ABEND                                                
068200                                                                          
068300     END-IF                                                               
068400     .                                                                    
068500*                                                                         
068600 D-LISTREQ SECTION.                                                       
068700*                                                                         
068800*****************************************************************         
068900*                                                                         
069000*    LISTS THE DIRECTORY OF AN OUTLOOK-MAILBOX, USING THE                 
069100*    MAILAPI.                                                             
069200*                                                                         
069300*    MAIL-USERID, MAIL-PASSWORD, MAIL- AND WORK-SERVER ARE TAKEN          
069400*    FROM MSEC.   FOLDER IS FETCHED FROM THE WZ1420D0 INPUT FILE.         
069500*                                                                         
069600*    IF RC FROM MAILAPI > X'0000', WITH ABEND U0100.                      
069700*                                                                         
069800*****************************************************************         
069900*                                                                         
070000     MOVE REQ-LIST                TO REQUEST                              
070100                                                                          
070200     MOVE MSEC-USERID             TO MAIL-USERID                          
070300     MOVE MSEC-PASSWORD           TO MAIL-PASSWORD                        
070400     MOVE MSEC-MAIL-SERVER        TO MAIL-SERVER                          
070500     MOVE MSEC-WORK-SERVER        TO WORK-SERVER                          
070600*                                 TO MAIL-SERVER                          
070700     MOVE A-FOLDER                TO FOLDER                               
070800                                                                          
070900     CALL FMAILAPI USING MAIL-COMMUNICATION-AREA                          
071000                         RETRIEVE-LIST                                    
071100     END-CALL                                                             
071200                                                                          
071300     IF  RETURN-CODE > ZEROES                                             
071400         MOVE '006 RC > 00 FROM FMAILAPI-LIST'                            
071500             TO ERRMSG                                                    
071600                                                                          
071700         PERFORM S01-ABEND                                                
071800                                                                          
071900     END-IF                                                               
072000     IF  NO-RECEIVED-REFID > 99                                           
072100         MOVE +1                    TO  RC                                
072200     ELSE                                                                 
072300         MOVE ZEROES                TO  RC                                
072400     END-IF                                                               
072500     .                                                                    
072600*                                                                         
072700 E-RETRENTRY SECTION.                                                     
072800*                                                                         
072900*****************************************************************         
073000*                                                                         
073100*                                                                         
073200*    SENDS INFORMATION IN AN DIRECTORY-ENTRY PLUS                         
073300*    DATA IN THE CTL-INPUT-FILE PLAS A RETURN-CODE-FIELD                  
073400*    TO AN EXIT-PROGRAM, THAT GIVE ITS RETURN-CODE IN                     
073500*    THE RETURN-CODE-FIELD.                                               
073600*                                                                         
073700*    THE NAME OF THE EXIT-PROGRAM IS TAKEN FROM THE CTL-FILE              
073800*                                                                         
073900*    THIS IS REPEATED FOR EACH VALID A-ENTRY                              
074000*                                                                         
074100*    WE INITIALIZE THE RETURN-CODE WITHTHE VALUE +00                      
074200*    BEFORE CALLING THE EXIT-PROGRAM, BUT AFTER THE LAST ENTRY            
074300*    HAS BEEN SENT TO THE EXIT-PROGRAM, WE PUT THE VALUE +99              
074400*    IN THE RETURN-CODE-FIELD TO INFORM THE EXIT-PROGRAM THAT IT          
074500*    IS TIME FOR CLEAN-UP.                                                
074600*                                                                         
074700*    IF +01 <=  (RETURN-CODE = NN) <= +10,THE INPUT TO THE                
074800*    EXIT-PROGRAM PLUS THE RETURN-CODE-FIELD ARE WRITTEN ON               
074900*    A FILE WITH DD-NAME = 'WZ1420NN'.                                    
075000*                                                                         
075100*    IF NN =+96, WE DO NOT WRITE THE INPUT TO THE                         
075200*    EXIT-PROGRAM BUT WE CONTINUES TO SEND INFORMAION ABOUT               
075300*    FOLLOWING RETRIVE-ENTRIES TO THE EXIT-PROGRAM, IF ANY                
075400*    A-ENTRY LEFT TO PROCESS.                                             
075500*    ORIGINATING FROM WZ1420D0                                            
075600*                                                                         
075700*    IF NN =+97, WE DO NOT WRITE THE INPUT TO THE                         
075800*    EXIT-PROGRAM AND CALL THE EXIT-PROGRAM A LAST TIME WITH              
075900*    THE VALUE +99 IN THE RETURN-CODE-FIELD.                              
076000*    AFTER THE RETURN FROM THE EXIT-PROGRAM, WE TERMINATE                 
076100*    THIS PROGRAM.                                                        
076200*                                                                         
076300*    IF NN =+98, THE EXIT-PROGRAM WANT US TO ABEND.                       
076400*                                                                         
076500*    ALL OTHER VALUES IN THE RETURN-CODE-FIELDS ARE INVALID               
076600*    AND WE ABEND.                                                        
076700*                                                                         
076800*                                                                         
076900*****************************************************************         
077000*                                                                         
077100     SET RETRIEVE-IX            TO  +1                                    
077200     SET RETRIEVE-IX2           TO  NO-RECEIVED-REFID                     
077300                                                                          
077400     PERFORM UNTIL RETRIEVE-IX > RETRIEVE-IX2                             
077500         MOVE RETRIEVE-LIST-ENTRY (RETRIEVE-IX) TO A-LIST-ENTRY           
077600                                                                          
077700*        -- A-DATE IS IN FORMAT "YYYY-MM-DD HH:MM:SS GMT"                 
077800         MOVE A-DATE (1 : 10)       TO DAYS-TIDATE1                       
077900         MOVE 'YYYY-MM-DD'          TO DAYS-KDDATFMT1                     
078000                                                                          
078100*        -- CURRENT DATE IS IN FORMAT "YYYYMMDDHHMMSSHH..."               
078200         MOVE FUNCTION CURRENT-DATE (1 : 8)                               
078300                                    TO DAYS-TIDATE2                       
078400         MOVE 'YYYYMMDD'            TO DAYS-KDDATFMT2                     
078500                                                                          
078600         CALL WZ20DAYS              USING DAYS-WZ20DAYS                   
078700                                                                          
078800         COMPUTE A-AGE              =   DAYS-KVDAYS                       
078900                                                                          
079000         MOVE ZEROES            TO  A-RETCODE                             
079100                                                                          
079200         CALL A-EXITPGM             USING A-AREA                          
079300                                          B-AREA                          
079400                                                                          
079500         IF  A-RETCODE = +97                                              
079600             SET RETRIEVE-IX            TO  RETRIEVE-IX2                  
079700         ELSE                                                             
079800             IF  A-RETCODE = +96                                          
079900               CONTINUE                                                   
080000             ELSE                                                         
080100                 IF  A-RETCODE = +98                                      
080200                     MOVE '007 ABEND-REQUEST FROM EXITPGM'                
080300                         TO ERRMSG                                        
080400                                                                          
080500                     PERFORM S01-ABEND                                    
080600                                                                          
080700                 END-IF                                                   
080800                 IF  A-RETCODE > +00 AND A-RETCODE < +11                  
080900                                                                          
081000                     PERFORM EA-WRITE                                     
081100                                                                          
081200                 ELSE                                                     
081300                     MOVE '008 INVALID RETCODE FROM EXITPGM'              
081400                         TO ERRMSG                                        
081500                                                                          
081600                     PERFORM S01-ABEND                                    
081700                                                                          
081800                 END-IF                                                   
081900             END-IF                                                       
082000         END-IF                                                           
082100         SET RETRIEVE-IX        UP BY +1                                  
082200     END-PERFORM                                                          
082300                                                                          
082400     MOVE +99               TO  A-RETCODE                                 
082500                                                                          
082600     CALL A-EXITPGM             USING A-AREA                              
082700                                                                          
082800     END-CALL                                                             
082900                                                                          
083000     PERFORM EB-CLOSE                                                     
083100     .                                                                    
083200*                                                                         
083300 EA-WRITE SECTION.                                                        
083400*                                                                         
083500*****************************************************************         
083600*                                                                         
083700*    WRITES ON THE WZ1420NN-FILES,  01 <= NN <= 10                        
083800*                                                                         
083900*****************************************************************         
084000*                                                                         
084100     MOVE OP-BYTE (A-RETCODE)    TO  OP2-FLGS                             
084200                                                                          
084300     EVALUATE A-RETCODE                                                   
084400                                                                          
084500         WHEN   +01                                                       
084600             IF  OP2-NOT-OPEN                                             
084700                                                                          
084800                 OPEN OUTPUT WZ1420D1                                     
084900                                                                          
085000             END-IF                                                       
085100                                                                          
085200             WRITE WZ1420D1-POST FROM A-AREA                              
085300                                                                          
085400         WHEN   +02                                                       
085500             IF  OP2-NOT-OPEN                                             
085600                                                                          
085700                 OPEN OUTPUT WZ1420D2                                     
085800                                                                          
085900             END-IF                                                       
086000                                                                          
086100             WRITE WZ1420D2-POST FROM A-AREA                              
086200                                                                          
086300         WHEN   +03                                                       
086400             IF  OP2-NOT-OPEN                                             
086500                                                                          
086600                 OPEN OUTPUT WZ1420D3                                     
086700                                                                          
086800             END-IF                                                       
086900                                                                          
087000             WRITE WZ1420D3-POST FROM A-AREA                              
087100                                                                          
087200         WHEN   +04                                                       
087300             IF  OP2-NOT-OPEN                                             
087400                                                                          
087500                 OPEN OUTPUT WZ1420D4                                     
087600                                                                          
087700             END-IF                                                       
087800                                                                          
087900             WRITE WZ1420D4-POST FROM A-AREA                              
088000                                                                          
088100         WHEN   +05                                                       
088200             IF  OP2-NOT-OPEN                                             
088300                                                                          
088400                 OPEN OUTPUT WZ1420D5                                     
088500                                                                          
088600             END-IF                                                       
088700                                                                          
088800             WRITE WZ1420D5-POST FROM A-AREA                              
088900                                                                          
089000         WHEN   +06                                                       
089100             IF  OP2-NOT-OPEN                                             
089200                                                                          
089300                 OPEN OUTPUT WZ1420D6                                     
089400                                                                          
089500             END-IF                                                       
089600                                                                          
089700             WRITE WZ1420D6-POST FROM A-AREA                              
089800                                                                          
089900         WHEN   +07                                                       
090000             IF  OP2-NOT-OPEN                                             
090100                                                                          
090200                 OPEN OUTPUT WZ1420D7                                     
090300                                                                          
090400             END-IF                                                       
090500                                                                          
090600             WRITE WZ1420D7-POST FROM A-AREA                              
090700                                                                          
090800         WHEN   +08                                                       
090900             IF  OP2-NOT-OPEN                                             
091000                                                                          
091100                 OPEN OUTPUT WZ1420D8                                     
091200                                                                          
091300             END-IF                                                       
091400                                                                          
091500             WRITE WZ1420D8-POST FROM A-AREA                              
091600                                                                          
091700         WHEN   +09                                                       
091800             IF  OP2-NOT-OPEN                                             
091900                                                                          
092000                 OPEN OUTPUT WZ1420D9                                     
092100                                                                          
092200             END-IF                                                       
092300                                                                          
092400             WRITE WZ1420D9-POST FROM A-AREA                              
092500                                                                          
092600         WHEN   +10                                                       
092700             IF  OP2-NOT-OPEN                                             
092800                                                                          
092900                 OPEN OUTPUT WZ1420DA                                     
093000                                                                          
093100             END-IF                                                       
093200                                                                          
093300             WRITE WZ1420DA-POST FROM A-AREA                              
093400                                                                          
093500         WHEN   OTHER                                                     
093600             MOVE '009 INVALID RETCODE FROM EXITPGM'                      
093700                                     TO ERRMSG                            
093800                                                                          
093900             PERFORM S01-ABEND                                            
094000                                                                          
094100     END-EVALUATE                                                         
094200                                                                          
094300     SET  OP2-OPEN               TO  TRUE                                 
094400     MOVE OP2-FLGS               TO  OP-BYTE (A-RETCODE)                  
094500     .                                                                    
094600*                                                                         
094700 EB-CLOSE SECTION.                                                        
094800*                                                                         
094900*****************************************************************         
095000*                                                                         
095100*                                                                         
095200*    CLOSES THE WZ1420NN-FILES,  01 <= NN <= 10, IF OPENED                
095300*                                                                         
095400*****************************************************************         
095500*                                                                         
095600     SET OP-IX                   TO  +1                                   
095700                                                                          
095800     PERFORM UNTIL OP-IX         > +10                                    
095900         MOVE OP-BYTE (OP-IX)        TO  OP2-FLGS                         
096000         SET SW-CLOSE                TO  OP-IX                            
096100                                                                          
096200         EVALUATE SW-CLOSE                                                
096300                                                                          
096400             WHEN   +01                                                   
096500                 IF  OP2-OPEN                                             
096600                                                                          
096700                     CLOSE WZ1420D1                                       
096800                                                                          
096900                 END-IF                                                   
097000             WHEN   +02                                                   
097100                 IF  OP2-OPEN                                             
097200                                                                          
097300                     CLOSE WZ1420D2                                       
097400                                                                          
097500                 END-IF                                                   
097600             WHEN   +03                                                   
097700                 IF  OP2-OPEN                                             
097800                                                                          
097900                     CLOSE WZ1420D3                                       
098000                                                                          
098100                 END-IF                                                   
098200                                                                          
098300             WHEN   +04                                                   
098400                 IF  OP2-OPEN                                             
098500                                                                          
098600                     CLOSE WZ1420D4                                       
098700                                                                          
098800                 END-IF                                                   
098900             WHEN   +05                                                   
099000                 IF  OP2-OPEN                                             
099100                                                                          
099200                     CLOSE WZ1420D5                                       
099300                                                                          
099400                 END-IF                                                   
099500             WHEN   +06                                                   
099600                 IF  OP2-OPEN                                             
099700                                                                          
099800                     CLOSE WZ1420D6                                       
099900                                                                          
100000                 END-IF                                                   
100100             WHEN   +07                                                   
100200                 IF  OP2-OPEN                                             
100300                                                                          
100400                     CLOSE WZ1420D7                                       
100500                                                                          
100600                 END-IF                                                   
100700             WHEN   +08                                                   
100800                 IF  OP2-OPEN                                             
100900                                                                          
101000                     CLOSE WZ1420D8                                       
101100                                                                          
101200                 END-IF                                                   
101300             WHEN   +09                                                   
101400                 IF  OP2-OPEN                                             
101500                                                                          
101600                     CLOSE WZ1420D9                                       
101700                                                                          
101800                 END-IF                                                   
101900             WHEN   +10                                                   
102000                 IF  OP2-OPEN                                             
102100                                                                          
102200                     CLOSE WZ1420DA                                       
102300                                                                          
102400                 END-IF                                                   
102500             WHEN   OTHER                                                 
102600                 MOVE '010 PROGRAM-ERROR'                                 
102700                                         TO ERRMSG                        
102800                                                                          
102900                 PERFORM S01-ABEND                                        
103000                                                                          
103100         END-EVALUATE                                                     
103200                                                                          
103300         SET OP2-NOT-OPEN            TO  TRUE                             
103400         MOVE OP2-FLGS               TO  OP-BYTE (OP-IX)                  
103500         SET OP-IX                   UP  BY +1                            
103600                                                                          
103700     END-PERFORM                                                          
103800     .                                                                    
103900*                                                                         
104000 F-DELETE SECTION.                                                        
104100*                                                                         
104200*****************************************************************         
104300*                                                                         
104400*    READS THE WZ1420NN-FILES,  01 <= NN <= 10                            
104500*                                                                         
104600*****************************************************************         
104700*                                                                         
104800     SET C-TYPE                  TO  B-IX                                 
104900                                                                          
105000     EVALUATE C-TYPE                                                      
105100                                                                          
105200         WHEN   +01                                                       
105300                                                                          
105400             OPEN INPUT WZ1420D1                                          
105500                                                                          
105600             SET EOF-NOT                 TO  TRUE                         
105700                                                                          
105800             READ WZ1420D1                   INTO S-AREA                  
105900                                                                          
106000                 AT END SET EOF-YES          TO  TRUE                     
106100                                                                          
106200             END-READ                                                     
106300                                                                          
106400             PERFORM UNTIL EOF-YES                                        
106500                                                                          
106600                 PERFORM FA-DELETE                                        
106700                                                                          
106800                 READ WZ1420D1                   INTO S-AREA              
106900                                                                          
107000                     AT END SET EOF-YES          TO  TRUE                 
107100                                                                          
107200                 END-READ                                                 
107300                                                                          
107400             END-PERFORM                                                  
107500                                                                          
107600             CLOSE WZ1420D1                                               
107700                                                                          
107800         WHEN   +02                                                       
107900                                                                          
108000             OPEN INPUT WZ1420D2                                          
108100                                                                          
108200             SET EOF-NOT                 TO  TRUE                         
108300                                                                          
108400             READ WZ1420D2                   INTO S-AREA                  
108500                                                                          
108600                 AT END SET EOF-YES          TO  TRUE                     
108700                                                                          
108800             END-READ                                                     
108900                                                                          
109000             PERFORM UNTIL EOF-YES                                        
109100                                                                          
109200                 PERFORM FA-DELETE                                        
109300                                                                          
109400                 READ WZ1420D2                   INTO S-AREA              
109500                                                                          
109600                     AT END SET EOF-YES          TO  TRUE                 
109700                                                                          
109800                 END-READ                                                 
109900                                                                          
110000             END-PERFORM                                                  
110100                                                                          
110200             CLOSE WZ1420D2                                               
110300                                                                          
110400         WHEN   +03                                                       
110500                                                                          
110600             OPEN INPUT WZ1420D3                                          
110700                                                                          
110800             SET EOF-NOT                 TO  TRUE                         
110900                                                                          
111000             READ WZ1420D3                   INTO S-AREA                  
111100                                                                          
111200                 AT END SET EOF-YES          TO  TRUE                     
111300                                                                          
111400             END-READ                                                     
111500                                                                          
111600             PERFORM UNTIL EOF-YES                                        
111700                                                                          
111800                 PERFORM FA-DELETE                                        
111900                                                                          
112000                 READ WZ1420D3                   INTO S-AREA              
112100                                                                          
112200                     AT END SET EOF-YES          TO  TRUE                 
112300                                                                          
112400                 END-READ                                                 
112500                                                                          
112600             END-PERFORM                                                  
112700                                                                          
112800             CLOSE WZ1420D3                                               
112900                                                                          
113000         WHEN   +04                                                       
113100                                                                          
113200             OPEN INPUT WZ1420D4                                          
113300                                                                          
113400             SET EOF-NOT                 TO  TRUE                         
113500                                                                          
113600             READ WZ1420D4                   INTO S-AREA                  
113700                                                                          
113800                 AT END SET EOF-YES          TO  TRUE                     
113900                                                                          
114000             END-READ                                                     
114100                                                                          
114200             PERFORM UNTIL EOF-YES                                        
114300                                                                          
114400                 PERFORM FA-DELETE                                        
114500                                                                          
114600                 READ WZ1420D4                   INTO S-AREA              
114700                                                                          
114800                     AT END SET EOF-YES          TO  TRUE                 
114900                                                                          
115000                 END-READ                                                 
115100                                                                          
115200             END-PERFORM                                                  
115300                                                                          
115400             CLOSE WZ1420D4                                               
115500                                                                          
115600         WHEN   +05                                                       
115700                                                                          
115800             OPEN INPUT WZ1420D5                                          
115900                                                                          
116000             SET EOF-NOT                 TO  TRUE                         
116100                                                                          
116200             READ WZ1420D5                   INTO S-AREA                  
116300                                                                          
116400                 AT END SET EOF-YES          TO  TRUE                     
116500                                                                          
116600             END-READ                                                     
116700                                                                          
116800             PERFORM UNTIL EOF-YES                                        
116900                                                                          
117000                 PERFORM FA-DELETE                                        
117100                                                                          
117200                 READ WZ1420D5                   INTO S-AREA              
117300                                                                          
117400                     AT END SET EOF-YES          TO  TRUE                 
117500                                                                          
117600                 END-READ                                                 
117700                                                                          
117800             END-PERFORM                                                  
117900                                                                          
118000             CLOSE WZ1420D5                                               
118100                                                                          
118200         WHEN   +06                                                       
118300                                                                          
118400             OPEN INPUT WZ1420D6                                          
118500                                                                          
118600             SET EOF-NOT                 TO  TRUE                         
118700                                                                          
118800             READ WZ1420D6                   INTO S-AREA                  
118900                                                                          
119000                 AT END SET EOF-YES          TO  TRUE                     
119100                                                                          
119200             END-READ                                                     
119300                                                                          
119400             PERFORM UNTIL EOF-YES                                        
119500                                                                          
119600                 PERFORM FA-DELETE                                        
119700                                                                          
119800                 READ WZ1420D6                   INTO S-AREA              
119900                                                                          
120000                     AT END SET EOF-YES          TO  TRUE                 
120100                                                                          
120200                 END-READ                                                 
120300                                                                          
120400             END-PERFORM                                                  
120500                                                                          
120600             CLOSE WZ1420D6                                               
120700                                                                          
120800         WHEN   +07                                                       
120900                                                                          
121000             OPEN INPUT WZ1420D7                                          
121100                                                                          
121200             SET EOF-NOT                 TO  TRUE                         
121300                                                                          
121400             READ WZ1420D7                   INTO S-AREA                  
121500                                                                          
121600                 AT END SET EOF-YES          TO  TRUE                     
121700                                                                          
121800             END-READ                                                     
121900                                                                          
122000             PERFORM UNTIL EOF-YES                                        
122100                                                                          
122200                 PERFORM FA-DELETE                                        
122300                                                                          
122400                 READ WZ1420D7                   INTO S-AREA              
122500                                                                          
122600                     AT END SET EOF-YES          TO  TRUE                 
122700                                                                          
122800                 END-READ                                                 
122900                                                                          
123000             END-PERFORM                                                  
123100                                                                          
123200             CLOSE WZ1420D7                                               
123300                                                                          
123400         WHEN   +08                                                       
123500                                                                          
123600             OPEN INPUT WZ1420D8                                          
123700                                                                          
123800             SET EOF-NOT                 TO  TRUE                         
123900                                                                          
124000             READ WZ1420D8                   INTO S-AREA                  
124100                                                                          
124200                 AT END SET EOF-YES          TO  TRUE                     
124300                                                                          
124400             END-READ                                                     
124500                                                                          
124600             PERFORM UNTIL EOF-YES                                        
124700                                                                          
124800                 PERFORM FA-DELETE                                        
124900                                                                          
125000                 READ WZ1420D8                   INTO S-AREA              
125100                                                                          
125200                     AT END SET EOF-YES          TO  TRUE                 
125300                                                                          
125400                 END-READ                                                 
125500                                                                          
125600             END-PERFORM                                                  
125700                                                                          
125800             CLOSE WZ1420D8                                               
125900                                                                          
126000         WHEN   +09                                                       
126100                                                                          
126200             OPEN INPUT WZ1420D9                                          
126300                                                                          
126400             SET EOF-NOT                 TO  TRUE                         
126500                                                                          
126600             READ WZ1420D9                   INTO S-AREA                  
126700                                                                          
126800                 AT END SET EOF-YES          TO  TRUE                     
126900                                                                          
127000             END-READ                                                     
127100                                                                          
127200             PERFORM UNTIL EOF-YES                                        
127300                                                                          
127400                 PERFORM FA-DELETE                                        
127500                                                                          
127600                 READ WZ1420D9                   INTO S-AREA              
127700                                                                          
127800                     AT END SET EOF-YES          TO  TRUE                 
127900                                                                          
128000                 END-READ                                                 
128100                                                                          
128200             END-PERFORM                                                  
128300                                                                          
128400             CLOSE WZ1420D9                                               
128500                                                                          
128600         WHEN   +10                                                       
128700                                                                          
128800             OPEN INPUT WZ1420DA                                          
128900                                                                          
129000             SET EOF-NOT                 TO  TRUE                         
129100                                                                          
129200             READ WZ1420DA                   INTO S-AREA                  
129300                                                                          
129400                 AT END SET EOF-YES          TO  TRUE                     
129500                                                                          
129600             END-READ                                                     
129700                                                                          
129800             PERFORM UNTIL EOF-YES                                        
129900                                                                          
130000                 PERFORM FA-DELETE                                        
130100                                                                          
130200                 READ WZ1420DA                   INTO S-AREA              
130300                                                                          
130400                     AT END SET EOF-YES          TO  TRUE                 
130500                                                                          
130600                 END-READ                                                 
130700                                                                          
130800             END-PERFORM                                                  
130900                                                                          
131000             CLOSE WZ1420DA                                               
131100                                                                          
131200         WHEN   OTHER                                                     
131300             MOVE '011 PROGRAM-ERROR'                                     
131400                                     TO ERRMSG                            
131500                                                                          
131600             PERFORM S01-ABEND                                            
131700                                                                          
131800     END-EVALUATE                                                         
131900                                                                          
132000     SET  OP2-OPEN               TO  TRUE                                 
132100     MOVE OP2-FLGS               TO  OP-BYTE (C-TYPE)                     
132200     .                                                                    
132300*                                                                         
132400 FA-DELETE SECTION.                                                       
132500*                                                                         
132600*****************************************************************         
132700*                                                                         
132800*    DELETES A MAIL IN OUR MAILBOX                                        
132900*                                                                         
133000*****************************************************************         
133100*                                                                         
133200     MOVE REQ-DELETE            TO REQUEST                                
133300     MOVE S-REFID               TO REFID                                  
133400                                                                          
133500     CALL FMAILAPI USING MAIL-COMMUNICATION-AREA                          
133600                                                                          
133700     IF  RETURN-CODE > 00                                                 
133800         MOVE '012 RC > 00 FROM FMAILAPI-DELETE'                          
133900             TO ERRMSG                                                    
134000                                                                          
134100         PERFORM S01-ABEND                                                
134200                                                                          
134300     END-IF                                                               
134400     .                                                                    
134500*                                                                         
134600 G-CLOSREQ SECTION.                                                       
134700*                                                                         
134800*****************************************************************         
134900*                                                                         
135000*    CLOSES OUR OUTLOOK-MAILBOX.                                          
135100*                                                                         
135200*****************************************************************         
135300*                                                                         
135400     MOVE REQ-CLOSE             TO REQUEST                                
135500                                                                          
135600     CALL FMAILAPI USING MAIL-COMMUNICATION-AREA                          
135700                                                                          
135800     END-CALL                                                             
135900                                                                          
136000     IF  RETURN-CODE > 00                                                 
136100         MOVE '013 RC > 00 FROM FMAILAPI-CLOSE'                           
136200             TO ERRMSG                                                    
136300                                                                          
136400         PERFORM S01-ABEND                                                
136500                                                                          
136600     END-IF                                                               
136700                                                                          
136800*    -- GIVE THE JAVA ENVIRONMENT TIME TO CLOSE DOWN                      
136900     CALL W009WAIT USING 20-SECONDS                                       
137000     .                                                                    
137100*                                                                         
137200****************************************************************          
137300*                                                                         
137400 S01-ABEND SECTION.                                                       
137500*                                                                         
137600*****************************************************************         
137700*                                                                         
137800*    ISSUES ALL ABENDS AND ASSOCIATED DISPLAYS.                           
137900*                                                                         
138000*****************************************************************         
138100*                                                                         
138200     DISPLAY 'WZ142000 M' ERRMSG                                          
138300     DISPLAY 'WZ142000 M' ERRMSG                                          
138400         UPON CONSOLE                                                     
138500                                                                          
138600     DISPLAY 'WZ142000 M999 *** ABNORMAL TERMINATION ***'                 
138700     DISPLAY 'WZ142000 M999 *** ABNORMAL TERMINATION ***'                 
138800         UPON CONSOLE                                                     
138900                                                                          
139000     CALL ABEND  USING RKOD-ABEND-UTAN-DUMP                               
139100                                                                          
139200     .                                                                    
