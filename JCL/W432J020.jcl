//W432J020 JOB (640W4320100W432J020,W100),'RTN W432V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//AMS     EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDB7CLU),DISP=SHR                            
//*                                                                             
//W432    EXEC W432P020                                                         
//W43220.DBOCTRL   DD *                                                         
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:43:32                             
                                                                                
  FUNCTION=RELOAD,USERLOAD,                                                     
  PCB=1,                                                                        
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//AMS     EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDB7ACLU),DISP=SHR                           
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDB7AK DD DSN=WG01.QASE.WDB7AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:43:32                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDB7,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//DBRC    EXEC WG01DBRC                                                         
//DBRC.SYSIN DD *                                                               
NOTIFY.UIC  DBD(WDB7) DDN(WDB7K) UDATA('VID RESTORE KÖR OM')                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W432J020                                         
//*                                                                             
