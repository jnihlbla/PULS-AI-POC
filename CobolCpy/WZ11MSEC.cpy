000100*** EDIT ALLOWED                                                          
000200*                                                                         
000300*                               PARAMETER AREA FOR WZ11MSEC.              
000400*                               FETCH SECURITY ID, PASSWORD AND           
000500*                               OTHER TECHNICAL DATA NEEDED TO            
000600*                               MAIL VIA AN OUTLOOK ID OR ACCESS          
000700*                               THE RELATED MAILBOX FOLDERS.              
000800*                                                                         
000900*                               INPUT ARGUMENTS:                          
001000*                               MSEC-MODNM - THE NAME OF THE              
001100*                                 TABLE MODULE CONTAINING                 
001200*                                 SECURITY INFO.                          
001300*                               MSEC-MAXRC - MAXIMUM RETURN COD           
001400*                                 ALLOWED FROM MAIL API.                  
001500*                                                                         
001600*                               OUTPUT ARGUMENTS:                         
001700*                               IF THE MODULE AND A MATCHING TABLE        
001800*                               ENTRY IS FOUND, MSEC-FLGS IS SET          
001900*                               TO "Y" AND THE OTHER FIELDS ARE           
002000*                               FILLED WITH DATA FROM THIS ENTRY          
002100*                               IF NOT, MSEC-FLGS IS SET TO "N"           
002200*                               AND AN ERROR MESSAGE IS WRITTEN           
002300*                               TO THE JOB LOG.                           
002400*                                                                         
002500 01     MSEC-AREA.                                                        
002600     03   MSEC-MODNM         PIC X(08).                                   
002700*                                 SECURITY TABLE MODULE NAME              
002800     03   MSEC-JOBID         PIC X(08).                                   
002900*                                 THE NAME OF THIS JOB                    
003000     03   MSEC-USOU1         PIC X(08).                                   
003100*                                 THE USER ID OF THIS JOB                 
003200     03   MSEC-USOU2         PIC X(08).                                   
003300*                                 USER ID 2 ?                             
003400     03   MSEC-PAOU2         PIC X(08).                                   
003500*                                 ?                                       
003600     03   MSEC-USERID        PIC X(24).                                   
003700*                                 OUTLOOK USER ID                         
003800     03   MSEC-PASSWORD      PIC X(24).                                   
003900*                                 OUTLOOK PASSWORD                        
004000     03   MSEC-MAXRC         PIC X(02).                                   
004100*                                 MAX RETURN CODE ALLOWED                 
004200     03   MSEC-MAIL-SERVER   PIC X(40).                                   
004300*                                 MAIL SERVER FOR ACCESSING               
004310*                                 OUTOOK                                  
004400     03   MSEC-WORK-SERVER   PIC X(40).                                   
004500*                                 WORK SERVER FOR ACCESSING               
004510*                                 OUTOOK                                  
004600     03   MSEC-PATH          PIC X(40).                                   
004700*                                 UNIX PATH FOR ATTACHMENTS ?             
004800     03   MSEC-OEOUT         PIC X(40).                                   
004900*                                 MAIL ADDRESS FOR ERROR MSGS ?           
005000     03   MSEC-FLGS          PIC X(01).                                   
005100*                                 RETURN FLAG FROM WZ11MSEC               
005200*                                 Y = OK, N = PROBLEMS                    
005300       88 MSEC-ANSWY                     VALUE 'Y'.                       
005400       88 MSEC-ANSWN                     VALUE 'N'.                       
005500     03   FILLER           PIC X(04).                                     
