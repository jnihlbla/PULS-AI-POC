000100* GENERATION OF COBOL HOST STRUCTURE FROM TZ4REKY-TAB                     
000200  01 TZ4REKY.                                                             
000300*              TZ4REKY                                                    
000400   03 IDOUTTYPE                         PIC X(15).                        
000500*              OUTPUTTYP                                                  
000600   03 IDOUTREC                          PIC X(30).                        
000700*              OUTPUTMOTTAGARE                                            
000800   03 IDLIST                            PIC X(10).                        
000900*              LISTIDENTITET                                              
001000   03 TIREGDAT                          PIC S9(7) COMP-3.                 
001100*              REGISTRERINGSDATUM (ÅÅMMDD)                                
001200   03 TIKLOCK                           PIC S9(9) COMP-3.                 
001300*              KLOCKSLAG (TTMMSSTH)                                       
001400   03 IDLOPNR                           PIC S9(3) COMP-3.                 
001500*              LÖPNUMMER                                                  
001600   03 KDOUTMETH                         PIC X(4).                         
001700*              OUTPUTMETOD                                                
001800   03 IDOUTDEST                         PIC X(60).                        
001900*              FYSISK OUTPUT DESTINATION                                  
002000   03 TIAAMMDD-RENS                     PIC S9(7) COMP-3.                 
002100*              RENSNINGSDATUM                                             
002200   03 KVCOPIES                          PIC X(1).                         
002300*              ANTAL COPIOR VID PRINTNING                                 
002400   03 FLCARRCNTL                        PIC X(1).                         
002500*              INGÅR STYRTECKEN I DATA?                                   
002600   03 IDPFDEF                           PIC X(8).                         
002700*              IBM PSF FORMSDEF,PAGEDEF                                   
002800   03 IDFORMSNM                         PIC X(8).                         
002900*              FORMS/BLANKETT-NAMN                                        
003000   03 TEVCOMST                          PIC X(20).                        
003100*              VCOM SENDERTAG                                             
003200   03 IDVCINIT                          PIC X(8).                         
003300*              VCOM INITIATOR PROGRAM NAMN                                
003400   03 TEFAX-1                           PIC X(50).                        
003500*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
003600   03 TEFAX-2                           PIC X(50).                        
003700*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
003800   03 TEFAX-3                           PIC X(50).                        
003900*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
004000   03 TEFAX-4                           PIC X(50).                        
004100*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
004200   03 TEFAX-5                           PIC X(50).                        
004300*              FAX TEXTRAD TILL FÖRSÄTTSBLAD                              
004400   03 IDMAIL-SENDER                     PIC X(60).                        
004500*              AVSÄNDANDE MAIL ADRESS                                     
004600   03 KVANTEX-PRINTAD                   PIC S9(1) COMP-3.                 
004700*              ANTAL GÅNGER LISTAN ÄR PRINTAD                             
004800   03 FLRULEMISS                        PIC X(1).                         
004900*              REGLER SAKNAS                                              
005000   03 FLACIF                            PIC X(1).                         
005100*              SKA ACIF ANVÄNDAS?                                         
005200*                                                                         
005300*** END OF VILMAII-COPY LENGTH= 493 OLD LENGTH=                           
