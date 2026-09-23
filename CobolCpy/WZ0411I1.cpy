000100 01  REQU-WZ0411I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WZ0411             
000300*                                 RESTART DISTRIBUTION MAINTENANC         
000400*                                 E                                       
000500     03 REQU-IDOUTTYPE-KEY   PIC X(15).                                   
000600*                                 OUTPUTTYP                               
000700*                                 OUTPUT TYPE                             
000800     03 REQU-IDOUTREC-KEY    PIC X(30).                                   
000900*                                 OUTPUTMOTTAGARE                         
001000*                                 OUTPUT RECEIVER                         
001100     03 REQU-IDLIST-KEY      PIC X(10).                                   
001200*                                 LISTIDENTITET                           
001300*                                 LIST IDENTITY                           
001400     03 REQU-TIREGDAT-KEY    PIC 9(6).                                    
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600*                                 REGISTRATION DATE (YYMMDD)              
001700     03 REQU-TIKLOCK-KEY     PIC 9(8).                                    
001800*                                 KLOCKSLAG (TTMMSSTH)                    
001900*                                 TIME OF DAY (HHMMSSTH)                  
002000     03 REQU-IDLOPNR-KEY     PIC X(3).                                    
002100*                                 LÖPNUMMER                               
002200*                                 SEQUENCE NUMBER                         
002300     03 REQU-KDOUTMETH       PIC X(4).                                    
002400*                                 OUTPUTMETOD                             
002500*                                 OUTPUT METHOD                           
002600     03 REQU-IDOUTDEST       PIC X(60).                                   
002700*                                 FYSISK OUTPUT DESTINATION               
002800*                                 PHYSICAL OUTPUT DESTINATION             
002900     03 REQU-KVCOPIES        PIC X.                                       
003000*                                 ANTAL COPIOR VID PRINTNING              
003100*                                 NUMBER OF PRINTED COPIES                
003200     03 REQU-FLCARRCNTL      PIC X.                                       
003300*                                 INGÅR STYRTECKEN I DATA?                
003400*                                 IS CARRRIAGE CONTROL INCLUDED?          
003500     03 REQU-IDPFDEF         PIC X(8).                                    
003600*                                 IBM PSF FORMSDEF,PAGEDEF                
003700*                                 IBM PSF FORMSDEF,PAGEDEF                
003800     03 REQU-IDFORMSNM       PIC X(8).                                    
003900*                                 FORMS/BLANKETT-NAMN                     
004000*                                 FORMS NAME                              
004100     03 REQU-TEVCOMST        PIC X(20).                                   
004200*                                 VCOM SENDERTAG                          
004300*                                 VCOM SENDERTAG                          
004400     03 REQU-IDVCINIT        PIC X(8).                                    
004500*                                 VCOM INITIATOR PROGRAM NAMN             
004600*                                 VCOM INITIATOR PROGRAM NAME             
004700     03 REQU-IDMAIL-SENDER   PIC X(60).                                   
004800*                                 AVSÄNDANDE MAIL ADRESS                  
004900*                                 MAIL ADDRESS OF SENDER                  
005000     03 REQU-TIAAMMDD-RENS   PIC 9(6).                                    
005100*                                 RENSNINGSDATUM                          
005200     03 REQU-TEFAX           OCCURS 5 TIMES                               
005300                             PIC X(50).                                   
005400*                                 FAX TEXTRAD TILL FÖRSÄTTSBLAD           
005500*                                 FAX INFO LINE                           
005600     03 REQU-FLRULEUSE       PIC X.                                       
005700*                                 FULL RESTART ENLIGT REGEL?              
005800*                                 FULL RESTART USING RULE?                
005900     03 REQU-FLACIF          PIC X.                                       
006000*                                 SKA ACIF ANVÄNDAS?                      
006100*                                 SHOULD ACIF BE USED?                    
006200*** END OF VILMAII-COPY LENGTH= 500 BYTES                                 
