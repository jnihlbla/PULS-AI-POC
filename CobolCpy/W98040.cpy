000100 01  40-W98040.                                                           
000200*                                 DATA TILL V444 FRÅN SOP                 
000300     03 40-IDPTYP-V444       PIC S9(4)           COMP                     
000400                             VALUE +2.                                    
000500*                                 POSTTYP V444                            
000600     03 40-IDRUTIN           PIC X(8)                                     
000700                             VALUE SPACES.                                
000800*                                 RUTINNAMN (GRUPP AV JOBB)               
000900     03 40-TIAPDAT           PIC 9(6)                                     
001000                             VALUE ZEROS.                                 
001100*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
001200*                                 (ÅÅMMDD)                                
001300     03 40-TIMINUT-START     PIC 9(4)                                     
001400                             VALUE ZEROS.                                 
001500*                                 STARTTID    (HH.MM)                     
001600     03 40-TIAPDAT-COM       PIC 9(6)                                     
001700                             VALUE ZEROS.                                 
001800*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
001900*                                 (ÅÅMMDD)                                
002000     03 40-TIMINUT-START-COM PIC 9(4)                                     
002100                             VALUE ZEROS.                                 
002200*                                 STARTTID    (HH.MM)                     
002300     03 40-KDPROCPRIO-COM    PIC X                                        
002400                             VALUE SPACE.                                 
002500*                                 PRIORITETSKOD                           
002600     03 40-KDOPCARB          PIC X(4)                                     
002700                             VALUE SPACES.                                
002800*                                 ARBETSSTATION I OPC                     
002900     03 FILLER               PIC X(55)                                    
003000                             VALUE SPACES.                                
003100     03 40-TIFDAT            PIC 9(6)                                     
003200                             VALUE ZEROS.                                 
003300*                                 FÄRDIGDATUM FÖR OUTPUTHANT              
003400*                                 (ÅÅMMDD)                                
003500     03 40-TIFMINUT          PIC 9(4)                                     
003600                             VALUE ZEROS.                                 
003700*                                 FÄRDIGTID FÖR OUTPUTHANT                
003800*                                 (HHMM)                                  
003900     03 40-KDPROCPRIO        PIC X                                        
004000                             VALUE SPACE.                                 
004100*                                 PRIORITETSKOD                           
004200     03 FILLER               PIC X(47)                                    
004300                             VALUE SPACES.                                
004400     03 40-TEOPTEXT          PIC X(18)                                    
004500                             VALUE SPACES.                                
004600*                                 OPERATÖRSMEDDELANDE                     
004700     03 FILLER               PIC X(13)                                    
004800                             VALUE SPACES.                                
004900     03 40-TIFDAT-2          PIC 9(6)                                     
005000                             VALUE ZEROS.                                 
005100*                                 FÄRDIGDATUM FÖR OUTPUTHANT              
005200*                                 (ÅÅMMDD)                                
005300     03 40-TIFMINUT-2        PIC 9(4)                                     
005400                             VALUE ZEROS.                                 
005500*                                 FÄRDIGTID FÖR OUTPUTHANT                
005600*                                 (HHMM)                                  
005700     03 FILLER               PIC X(50)                                    
005800                             VALUE SPACES.                                
005900*                                                                         
006000*** END COPY W98040CCC0  LENGTH=239                                       
