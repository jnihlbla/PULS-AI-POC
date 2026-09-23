//W114J010 JOB (640W1140100W114J010,W100),'RTN W114D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*                                                                             
//IDCAMS  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD SYSOUT=*                                                          
 DELETE (WG01.QASE.WDF7V) NONVSAM PURGE                                         
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDF7V  DD DSN=WG01.QASE.WDF7V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(2048,(6300,315),,CONTIG,ROUND),                             
//           DATACLAS=MVOL,MGMTCLAS=TP0                                         
//*                                                                             
//W114    EXEC W114P010                                                         
//W11410.DBOCTRL   DD *                                                         
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:58                             
                                                                                
  FUNCTION=RELOAD,USERLOAD,                                                     
  PCB=1,                                                                        
  HDSORT=YES,                                                                   
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDF7V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDF7ACLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDF7AK DD DSN=WG01.QASE.WDF7AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:58                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDF7,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//DBRC    EXEC WG01DBRC                                                         
//DBRC.SYSIN DD *                                                               
NOTIFY.UIC  DBD(WDF7) DDN(WDF7V) UDATA('VID RESTORE KÖR OM')                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J010                                         
