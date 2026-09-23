//W517V1RE JOB (650W5170100W517V1RE,W100),'RTN W517V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W517V1,MAXRC=8                                        
//*                                                                             
//***********************************************************                   
//*  KOPIERING AV FIL ÅT PV:S MARKNADSFÖRINGSSYSTEM SHARK.                      
//*  PV BEHÖVER AKTUELL INFORMATION 4 - 6 GGR /ÅR.                              
//* (KONTAKTMAN PÅ PV KONSULT OLE JOHANSEN MEMOID EXT.CYBEXOJ)                  
//*                                                                             
//*  OM GENER1-STEGET ABENDAR                                                   
//*  GÖR END PÅ W517V1RE I SOP !!!   PV BEHÖVER EJ KONTAKTAS!                   
//**********************************************************                    
//GENER1  EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W517.W517V1.W51721(+0),DISP=(OLD,KEEP,KEEP)                  
//SYSUT2   DD  DSN=WUT.W517V1.W51721(+1),DISP=(NEW,CATLG,DELETE),               
//             MGMTCLAS=BACKUPC,SPACE=(23435,(15,10),RLSE)                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W517V1RE                                         
