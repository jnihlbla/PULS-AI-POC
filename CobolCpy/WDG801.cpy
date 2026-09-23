000100 01  LIST-WDG801.                                                         
000200*                                 ÅTERSTARTSREGISTER                      
000300*                                 LISTINFORMATIONS-SEGMENT                
000400*                                 FYSISK NYCKEL: WDG801KY                 
000500*                                 (IDLTERM + TIREGDAT + TIKLOCK)          
000600     03 LIST-IDLTERM         PIC X(8).                                    
000700*                                 LOGISKT TERMINALNAMN                    
000800*                                 IDENTITY OF LOGICAL TERMINAL            
000900     03 LIST-TIREGDAT        PIC S9(7)           COMP-3.                  
001000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001100*                                 REGISTRATION DATE (YYMMDD)              
001200     03 LIST-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
001300*                                 TID LAGRAT SOM 9-KOMPLEMENT             
001400*                                 TIME SAVED AS 9-COMPLEMENT              
001500     03 LIST-IDLIST          PIC X(10).                                   
001600*                                 LISTIDENTITET                           
001700*                                 LIST IDENTITY                           
001800     03 LIST-IDPRTLST        PIC X(8).                                    
001900*                                 LOGISK PRINTER+LISTA IDENTITET          
002000*                                 LOGICAL PRINTER+LIST IDENTITY           
002100     03 LIST-FLSKRIV         PIC X.                                       
002200*                                 JA = ÅTERSTART AV BEBÄRD LISTA          
002300     03 LIST-KVANTEX-PRINTAD PIC S9              COMP-3.                  
002400*                                 ANTAL GÅNGER LISTAN ÄR PRINTAD          
002500     03 LIST-KVLL            PIC S9(4)           COMP.                    
002600*                                 LRECL I ETT VARIABELT RECORD            
002700*                                 LRECL IN A VARIABLE RECORD              
002800     03 LIST-W006PRTY.                                                    
002900*                                 STYRPARAMETRAR TILL                     
003000*                                 PRINTER SUBPROGRAM                      
003100        05 LIST-IDPRTSPO     PIC X(8).                                    
003200*                                 SPOOL PRINTER SID-STORLEK               
003300*                                 SPOOL PRINTER PAGE SIZE                 
003400        05 LIST-KDCOPIES     PIC X.                                       
003500*                                 ANTAL COPIOR VID PRINTNING              
003600*                                 NUMBER OF PRINTING COPIES               
003700        05 LIST-KDFORMS      PIC X.                                       
003800*                                 KOD FÖR FORMSNUMMER                     
003900*                                 CODE FOR FORMSNUMBER                    
004000        05 LIST-IDPFDEF      PIC X(8).                                    
004100*                                 IBM PSF FORMSDEF,PAGEDEF                
004200*                                 IBM PSF FORMSDEF,PAGEDEF                
004300        05 LIST-IDCOPYG      PIC X(8).                                    
004400*                                 COPYGRUPP IBM PSF                       
004500*                                 COPYGROUP IBM PSF                       
004600        05 LIST-FILLER       PIC X(24).                                   
004700        05 LIST-IDTFX        PIC X(20).                                   
004800*                                 TELEFAXNUMMER                           
004900*                                 FAXNUMBER                               
005000        05 LIST-TEFAX        OCCURS 5 TIMES                               
005100                             PIC X(50).                                   
005200*                                 FAX TEXTRAD TILL FÖRSÄTTSBLAD           
005300*                                 FAX INFO LINE                           
005400     03 LIST-IDPRTTYP        PIC X(5).                                    
005500*                                 SPOOL PRINTER TYP                       
005600*                                 SPOOL PRINTER TYPE                      
005700     03 LIST-IDPGM           PIC X(8).                                    
005800*                                 PROGRAM IDENTITET                       
005900*                                 PROGRAM INTENTITY                       
006000*** END OF VILMAII-COPY LENGTH= 372 BYTES                                 
