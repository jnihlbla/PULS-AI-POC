//W513V1RE JOB (650W5130100W513V1RE,W100),'RTN W513V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W513V1,MAXRC=8                                        
//*                                                                             
//* KOPIERA FIL TILL VIOS                                                       
//*                                                                             
//GENER02 EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W513.W513V1.W51343(+0),DISP=SHR                              
//SYSUT2   DD  DSN=WXTR.W513V1.WXTR5D(+1),DISP=(NEW,CATLG,DELETE),              
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W513V1RE                                         
