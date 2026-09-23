000100 01  REQU-WZ0402I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WZ0402             
000300*                                 DISTRIBUTION RULES MAINTENANCE          
000400     03 REQU-IDOUTTYPE-KEY   PIC X(15).                                   
000500*                                 OUTPUTTYP                               
000600*                                 OUTPUT TYPE                             
000700     03 REQU-IDOUTREC-FROM-KEY                                            
000800                             PIC X(30).                                   
000900*                                 OUTPUTMOTTAGARE (FR.O.M)                
001000*                                 OUTPUT RECEIVER (FROM)                  
001100     03 REQU-IDOUTREC-TO-KEY PIC X(30).                                   
001200*                                 OUTPUTMOTTAGARE (T.O.M)                 
001300*                                 OUTPUT RECEIVER (TO)                    
001400     03 REQU-KDOUTMETH       OCCURS 15 TIMES                              
001500                             PIC X(4).                                    
001600*                                 OUTPUTMETOD                             
001700*                                 OUTPUT METHOD                           
001800     03 REQU-IDOUTDEST       OCCURS 15 TIMES                              
001900                             PIC X(60).                                   
002000*                                 FYSISK OUTPUT DESTINATION               
002100*                                 PHYSICAL OUTPUT DESTINATION             
002200     03 REQU-KVCOPIES        OCCURS 15 TIMES                              
002300                             PIC X.                                       
002400*                                 ANTAL COPIOR VID PRINTNING              
002500*                                 NUMBER OF PRINTED COPIES                
002600     03 REQU-FLCARRCNTL      OCCURS 15 TIMES                              
002700                             PIC X.                                       
002800*                                 INGÅR STYRTECKEN I DATA?                
002900*                                 IS CARRRIAGE CONTROL INCLUDED?          
003000     03 REQU-IDPFDEF         OCCURS 15 TIMES                              
003100                             PIC X(8).                                    
003200*                                 IBM PSF FORMSDEF,PAGEDEF                
003300*                                 IBM PSF FORMSDEF,PAGEDEF                
003400     03 REQU-IDFORMSNM       OCCURS 15 TIMES                              
003500                             PIC X(8).                                    
003600*                                 FORMS/BLANKETT-NAMN                     
003700*                                 FORMS NAME                              
003800     03 REQU-TEFAX           OCCURS 5 TIMES                               
003900                             PIC X(50).                                   
004000*                                 FAX TEXTRAD TILL FÖRSÄTTSBLAD           
004100*                                 FAX INFO LINE                           
004200     03 REQU-TEVCOMST        OCCURS 15 TIMES                              
004300                             PIC X(20).                                   
004400*                                 VCOM SENDERTAG                          
004500*                                 VCOM SENDERTAG                          
004600     03 REQU-IDVCINIT        OCCURS 15 TIMES                              
004700                             PIC X(8).                                    
004800*                                 VCOM INITIATOR PROGRAM NAMN             
004900*                                 VCOM INITIATOR PROGRAM NAME             
005000     03 REQU-KVDAGAR-RESEND  PIC 9(2).                                    
005100*                                 SPARA ANT. DAGAR FÖR OMSÄNDNING         
005200*                                 NO. OF DAYS TO KEEP FOR RESEND          
005300     03 REQU-IDMAIL-SENDER   PIC X(60).                                   
005400*                                 AVSÄNDANDE MAIL ID/PWD                  
005500*                                 ID/PWD OF SENDER                        
005600     03 REQU-FLACIF          OCCURS 15 TIMES                              
005700                             PIC X.                                       
005800*                                 SKA ACIF ANVÄNDAS?                      
005900*                                 SHOULD ACIF BE USED?                    
006000     03 REQU-TENOTE          PIC X(300).                                  
006100*                                 NOTERINGSFÄLT                           
006200*                                 NOTE FIELD                              
006300*** END OF VILMAII-COPY LENGTH= 2352 BYTES                                
